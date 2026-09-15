# ======================================================================================================================
# 31-OnlineAppendix.R -- the online appendix: numbers, floats and references for a text that is rendered twice
# ======================================================================================================================
#
# 31-OnlineAppendix.qmd holds the appendix's text. Quarto renders it twice: to html, which is the runbook, and to a
# LaTeX fragment the paper's Overleaf project inputs. This library supplies what the text cannot write for itself --
# the numbers it cites, read from the tibbles 30 writes beside its exhibits; the floats, which point at 30's tables,
# notes and figures by name; the cross-references -- and the checks that every one of them resolved.
#
# THREE KINDS OF FUNCTION. Compute functions return a tibble and print nothing, and report functions print through
# cli and return their input invisibly, as in every library here. EMITTERS are the third kind: they write markup for
# a `results: asis` chunk, or return it for inline R -- LaTeX in the LaTeX pass, markdown in the html pass -- and
# never print through cli, because what they write lands in the appendix itself, where a message would be typeset.
#
# THE HTML PASS SHOWS EACH EXHIBIT IN PLACE. A figure is 30's png copy; a table is 30's own tabular, converted to
# html by the Pandoc that Quarto itself runs, after a few normalisations the converter needs (section 4). Both
# passes therefore set the same content, and the html numbers every exhibit as the paper does.
#
# ONE PIECE OF STATE, AND WHY. The text calls an emitter wherever a number, a reference or a float belongs, some
# forty times. Passing the float registry and the resolved numbers into every one of those calls would put
# bookkeeping into the appendix's own sentences. So Configuration builds the context once, on the page, and stores
# it in .oa_state; the emitters read it there and record what they emitted, which is what Validation checks.


# 1. Render state ------------------------------------------------------------------------------------------------------

.oa_state <- new.env(parent = emptyenv())

#' Store a value in the render's state
#' @param .what Character. The slot: Ctx, Numbers, Emitted, Cache, Shown, Pandoc, Residue or Builds.
#' @param .value Any. What to store.
#' @return Invisibly, .value.
oa_state_put <- function(.what, .value) {
  if (FALSE) {
    .what  <- "Emitted"
    .value <- character(0)
  }
  assign(
    x     = .what,
    value = .value,
    envir = .oa_state
  )
  invisible(.value)
}

#' Read a value from the render's state
#' @param .what Character. The slot: Ctx, Numbers, Emitted, Cache, Shown, Pandoc, Residue or Builds.
#' @return The stored value, or NULL where the slot was never filled.
oa_state_get <- function(.what) {
  if (FALSE) .what <- "Emitted"
  found_ <- exists(
    x        = .what,
    envir    = .oa_state,
    inherits = FALSE
  )
  if (!found_) return(NULL)
  get(
    x        = .what,
    envir    = .oa_state,
    inherits = FALSE
  )
}

#' Empty the record of this render: context, numbers, emitted floats and cached tibbles
#'
#' Called at the top of the runbook, and once when this file is sourced. Within one pass the body emits each float
#' once and Validation counts them; run a second time in an interactive session, the body would count every float
#' twice without the reset.
#'
#' @return Invisibly NULL.
oa_state_reset <- function() {
  oa_state_put(
    .what  = "Ctx",
    .value = NULL
  )
  oa_state_put(
    .what  = "Numbers",
    .value = NULL
  )
  oa_state_put(
    .what  = "Emitted",
    .value = character(0)
  )
  oa_state_put(
    .what  = "Cache",
    .value = list()
  )
  oa_state_put(
    .what  = "Shown",
    .value = logical(0)
  )
  oa_state_put(
    .what  = "Pandoc",
    .value = NULL
  )
  oa_state_put(
    .what  = "Residue",
    .value = character(0)
  )
  oa_state_put(
    .what  = "Builds",
    .value = character(0)
  )
  invisible(NULL)
}

#' Is this the LaTeX pass?
#'
#' Quarto sets the pandoc target for knitr, so knitr's own test is the reliable one: TRUE while the fragment for
#' Overleaf is being written, FALSE while the runbook is.
#'
#' @return Logical scalar.
oa_is_tex <- function() {
  isTRUE(knitr::is_latex_output())
}

oa_state_reset()


# 2. LaTeX helpers: escaping, labels, references -----------------------------------------------------------------------

#' Escape the characters LaTeX reads as commands
#'
#' For captions and for the notes of the static tables: text an author types as plain prose, which the LaTeX pass
#' must set literally. The backslash is parked on a control character first and restored last, because the escapes
#' added for the braces would otherwise be escaped a second time.
#'
#' @param .x Character.
#' @return Character, safe in running LaTeX text.
oa_tex_escape <- function(.x) {
  if (FALSE) .x <- c("R&D", "50% of $1", "a_b {c}", "back\\slash")
  out_ <- gsub(
    pattern     = "\\",
    replacement = "\001",
    x           = .x,
    fixed       = TRUE
  )
  for (ch_ in c("&", "%", "$", "#", "_", "{", "}")) {
    out_ <- gsub(
      pattern     = ch_,
      replacement = paste0("\\", ch_),
      x           = out_,
      fixed       = TRUE
    )
  }
  subs_ <- c("\001" = "\\textbackslash{}", "~" = "\\textasciitilde{}", "^" = "\\textasciicircum{}")
  for (k_ in names(subs_)) {
    out_ <- gsub(
      pattern     = k_,
      replacement = subs_[[k_]],
      x           = out_,
      fixed       = TRUE
    )
  }
  out_
}

#' The LaTeX label and the html anchor of an exhibit
#'
#' Named, not numbered, as every exhibit in this project is: tab:oa-ClassTransformer rather than tab:c2. The
#' appendix's own exhibits carry the oa- prefix so they cannot collide with the manuscript's; a manuscript exhibit is
#' labelled by its bare name, tab:Categories, which is the convention the manuscript's labels must follow for a
#' reference from here to resolve. The html anchor is built the other way round, oa-tab-..., because Quarto reads an
#' id starting fig- as a figure of its own cross-reference system.
#'
#' @param .name Character. The exhibit's stem.
#' @param .kind Character. "table" or "figure".
#' @param .scope Character. "oa" for this appendix, "main" for the manuscript.
#' @return List: Label (LaTeX) and Anchor (html id).
oa_label <- function(.name, .kind, .scope) {
  if (FALSE) {
    .name  <- "ClassTransformer"
    .kind  <- "table"
    .scope <- "oa"
  }
  pre_ <- if (identical(.kind, "table")) "tab" else "fig"
  if (identical(.scope, "oa")) {
    return(list(
      Label  = paste0(pre_, ":oa-", .name),
      Anchor = paste0("oa-", pre_, "-", .name)
    ))
  }
  list(
    Label  = paste0(pre_, ":", .name),
    Anchor = paste0("main-", pre_, "-", .name)
  )
}

#' The markup of a reference, for either pass
#'
#' The LaTeX pass returns a raw inline span, so pandoc hands the reference to LaTeX untouched and the tie keeps the
#' word and the number on one line. The html pass returns a link to the exhibit in the runbook, carrying the number the
#' paper will print; a manuscript exhibit, which the runbook does not show, is named instead.
#'
#' @param .name Character. The exhibit's stem.
#' @param .kind Character. "table" or "figure".
#' @param .scope Character. "oa" or "main".
#' @param .number Character. The exhibit's number in the appendix, C3; NA for a manuscript exhibit.
#' @return Character scalar.
oa_ref_markup <- function(.name, .kind, .scope, .number) {
  if (FALSE) {
    .name   <- "ClassTransformer"
    .kind   <- "table"
    .scope  <- "oa"
    .number <- "C3"
  }
  lab_  <- oa_label(
    .name  = .name,
    .kind  = .kind,
    .scope = .scope
  )
  word_ <- if (identical(.kind, "table")) "Table" else "Figure"
  if (oa_is_tex()) return(paste0("`", word_, "~\\ref{", lab_$Label, "}`{=latex}"))
  if (identical(.scope, "main")) return(paste0(word_, " *", .name, "*"))
  paste0("[", word_, " ", .number, "](#", lab_$Anchor, ")")
}

#' A reference to one of this appendix's exhibits, for inline R in the text
#'
#' The kind comes from the registry, so the text names the exhibit and never has to say whether it is a table or a
#' figure. A name the registry does not hold aborts, which is what turns a typo into an error rather than into a
#' question mark in the typeset paper.
#'
#' @param .name Character. The exhibit's stem, as registered in Configuration.
#' @return Character scalar: markup for the current pass.
oa_ref <- function(.name) {
  if (FALSE) .name <- "ClassTransformer"
  ctx_ <- oa_state_get(.what = "Ctx")
  if (is.null(ctx_)) cli::cli_abort("No appendix context: oa_context_set() has not run.")
  row_ <- ctx_$Files[ctx_$Files$Name == .name, , drop = FALSE]
  if (nrow(row_) != 1L) cli::cli_abort("{.val {(.name)}} is not in the float registry.")
  oa_ref_markup(
    .name   = .name,
    .kind   = row_$Kind,
    .scope  = "oa",
    .number = row_$Number
  )
}

#' A reference to one of the manuscript's exhibits
#'
#' The manuscript is not rendered here, so its exhibits are not in the registry and their kind is stated. The label
#' referred to is tab:<name> or fig:<name>; the manuscript must carry it for the reference to resolve in Overleaf.
#'
#' @param .name Character. The manuscript exhibit's stem.
#' @param .kind Character. "table" or "figure".
#' @return Character scalar: markup for the current pass.
oa_ref_main <- function(.name, .kind) {
  if (FALSE) {
    .name <- "Categories"
    .kind <- "table"
  }
  if (!.kind %in% c("table", "figure")) cli::cli_abort("{.arg .kind} is {.val table} or {.val figure}.")
  oa_ref_markup(
    .name   = .name,
    .kind   = .kind,
    .scope  = "main",
    .number = NA_character_
  )
}


# 3. Numbers: every number the text cites, read from the exhibit tibbles -----------------------------------------------

#' Read one of 30's exhibit tibbles, once per render
#'
#' 30 writes the tibble behind every exhibit to Data/<name>.parquet, and those files are what a number in this text
#' is taken from, so a sentence and the table beside it cannot disagree. A missing file returns NULL rather than
#' failing: the caller decides whether a missing source is fatal or a marker in the text.
#'
#' @param .name Character. The exhibit's stem.
#' @param .dir_data Character. 30's Output/Data.
#' @return Tibble, or NULL where the file does not exist.
oa_read_exhibit <- function(.name, .dir_data) {
  if (FALSE) {
    .name     <- "ClassTransformer"
    .dir_data <- here::here("2_output", "30-FinalExhibits", "Output", "Data")
  }
  key_   <- paste0("Data:", .name)
  cache_ <- oa_state_get(.what = "Cache")
  if (!is.null(cache_[[key_]])) return(cache_[[key_]])
  paths_ <- fs::path(.dir_data, paste0(.name, ".parquet"))
  hit_   <- paths_[fs::file_exists(paths_)]
  if (length(hit_) == 0L) return(NULL)
  tab_ <- tibble::as_tibble(arrow::read_parquet(file = hit_[[1L]]))
  cache_[[key_]] <- tab_
  oa_state_put(
    .what  = "Cache",
    .value = cache_
  )
  tab_
}

#' Format a number for the text
#'
#' @param .x Numeric scalar.
#' @param .format Character. "count" (rounded, thousands separated), "prop3" (three decimals), "pct1" (times one
#'   hundred, one decimal, no percent sign -- the text writes it), "num1" or "num2" (one or two decimals).
#' @return Character scalar.
oa_format_number <- function(.x, .format) {
  if (FALSE) {
    .x      <- 0.88234
    .format <- "prop3"
  }
  switch(.format,
    count = format(
      round(.x),
      big.mark   = ",",
      scientific = FALSE,
      trim       = TRUE
    ),
    count100 = format(
      round(.x, -2),
      big.mark   = ",",
      scientific = FALSE,
      trim       = TRUE
    ),
    prop3 = formatC(
      .x,
      format = "f",
      digits = 3L
    ),
    pct1 = formatC(
      100 * .x,
      format = "f",
      digits = 1L
    ),
    num1 = formatC(
      .x,
      format   = "f",
      digits   = 1L,
      big.mark = ","
    ),
    num2 = formatC(
      .x,
      format   = "f",
      digits   = 2L,
      big.mark = ","
    ),
    cli::cli_abort("Unknown number format {.val {(.format)}}.")
  )
}

#' Resolve one number: read the exhibit, filter its rows, reduce, format
#'
#' @param .exhibit Character. The exhibit's stem.
#' @param .column Character. The column holding the value.
#' @param .where Character. A filter over the tibble's own columns, evaluated as written.
#' @param .fun Character. "one" requires exactly one matching row; "min", "max", "sum" and "mean" reduce several.
#' @param .format Character. See oa_format_number().
#' @param .dir_data Character. 30's Output/Data.
#' @return One-row tibble: Value (character, NA where unresolved) and Status ("ok" or the reason).
oa_number_one <- function(.exhibit, .column, .where, .fun, .format, .dir_data) {
  if (FALSE) {
    .exhibit  <- "ClassTransformer"
    .column   <- "F1"
    .where    <- "Kind == 'total'"
    .fun      <- "one"
    .format   <- "prop3"
    .dir_data <- here::here("2_output", "30-FinalExhibits", "Output", "Data")
  }
  fail_ <- function(.why) {
    tibble::tibble(
      Value  = NA_character_,
      Status = .why
    )
  }
  tab_ <- oa_read_exhibit(
    .name     = .exhibit,
    .dir_data = .dir_data
  )
  if (is.null(tab_)) return(fail_(.why = "exhibit not built"))
  if (!.column %in% names(tab_)) return(fail_(.why = paste0("no column ", .column)))
  sel_ <- tryCatch(
    dplyr::filter(tab_, !!rlang::parse_expr(.where)),
    error = function(.e) NULL
  )
  if (is.null(sel_)) return(fail_(.why = "filter does not evaluate"))
  v_ <- sel_[[.column]]
  if (identical(.fun, "one")) {
    if (length(v_) != 1L) return(fail_(.why = paste0("filter matched ", length(v_), " rows")))
  } else {
    if (length(v_) == 0L) return(fail_(.why = "filter matched no rows"))
    if (!.fun %in% c("min", "max", "sum", "mean")) return(fail_(.why = paste0("unknown reduction ", .fun)))
    v_ <- do.call(
      what = .fun,
      args = list(v_, na.rm = TRUE)
    )
  }
  if (!is.numeric(v_) || is.na(v_)) return(fail_(.why = "value missing or not numeric"))
  tibble::tibble(
    Value  = oa_format_number(
      .x      = v_,
      .format = .format
    ),
    Status = "ok"
  )
}

#' Resolve every number the text cites, and keep them for oa_n()
#'
#' One row per number: which exhibit tibble it comes from, which column, which rows, how several rows reduce to one
#' value, and how it is printed. A number that cannot be resolved -- the exhibit is not built, the column has gone,
#' the filter matched the wrong number of rows -- is kept with the reason, so the report says why rather than that.
#'
#' @param .spec Tibble: Key, Exhibit, Column, Fun, Format, Where.
#' @param .dir_data Character. 30's Output/Data.
#' @return Tibble: .spec plus Value and Status.
oa_numbers_set <- function(.spec, .dir_data) {
  if (FALSE) {
    .spec     <- tab_numbers_spec
    .dir_data <- here::here("2_output", "30-FinalExhibits", "Output", "Data")
  }
  need_ <- c("Key", "Exhibit", "Column", "Fun", "Format", "Where")
  miss_ <- setdiff(need_, names(.spec))
  if (length(miss_) > 0L) cli::cli_abort("The number specification lacks {.field {miss_}}.")
  dup_ <- unique(.spec$Key[duplicated(.spec$Key)])
  if (length(dup_) > 0L) cli::cli_abort("Number key{?s} declared twice: {.val {dup_}}.")
  res_ <- purrr::map(seq_len(nrow(.spec)), \(.i) {
    oa_number_one(
      .exhibit  = .spec$Exhibit[.i],
      .column   = .spec$Column[.i],
      .where    = .spec$Where[.i],
      .fun      = .spec$Fun[.i],
      .format   = .spec$Format[.i],
      .dir_data = .dir_data
    )
  }) |>
    purrr::list_rbind()
  out_ <- dplyr::bind_cols(.spec, res_)
  oa_state_put(
    .what  = "Numbers",
    .value = out_
  )
  out_
}

#' One number, for inline R in the text
#'
#' A key the specification does not hold aborts, so a typo is an error and not a blank. A key it holds but could not
#' resolve returns a bold marker carrying the key, visible in both renders and listed by the report.
#'
#' @param .key Character. The number's key in the specification.
#' @return Character scalar: the formatted value, or the marker.
oa_n <- function(.key) {
  if (FALSE) .key <- "NLabelled"
  tab_ <- oa_state_get(.what = "Numbers")
  if (is.null(tab_)) cli::cli_abort("No numbers resolved: oa_numbers_set() has not run.")
  row_ <- tab_[tab_$Key == .key, , drop = FALSE]
  if (nrow(row_) != 1L) cli::cli_abort("{.val {(.key)}} is not in the number specification.")
  if (identical(row_$Status, "ok")) return(row_$Value)
  paste0("**[?", .key, "]**")
}

#' Report the numbers: every value with its source, and the ones that did not resolve
#'
#' @param .tab Tibble from oa_numbers_set().
#' @param .strict Logical. TRUE aborts where any number is unresolved.
#' @return Invisibly, .tab.
oa_report_numbers <- function(.tab, .strict) {
  if (FALSE) {
    .tab    <- tab_numbers
    .strict <- FALSE
  }
  show_ <- .tab |>
    dplyr::transmute(
      Key     = .data$Key,
      Value   = dplyr::coalesce(.data$Value, "-"),
      Status  = .data$Status,
      Exhibit = .data$Exhibit,
      Column  = .data$Column,
      Fun     = .data$Fun
    )
  tbl_say(
    .tab   = show_,
    .title = "Every number the text cites, and where it comes from"
  )
  bad_ <- .tab$Key[.tab$Status != "ok"]
  if (length(bad_) == 0L) {
    cli::cli_alert_success("All {nrow(.tab)} numbers resolved.")
  } else {
    cli::cli_alert_warning("{length(bad_)} of {nrow(.tab)} numbers unresolved, set in the text as [?key]: {.val {bad_}}.")
    if (isTRUE(.strict)) cli::cli_abort("Strict: an unresolved number stops the render.")
  }
  cli::cli_alert_info("A number marked unresolved is a missing exhibit or a changed column in 30, not a typo here.")
  invisible(.tab)
}


#' The resolved numbers as LaTeX macros
#'
#' The manuscript cites several of the numbers this appendix cites -- the labelled sample, the classifier's scores --
#' and a number typed there is a copy that goes stale at the next re-export. Written here as macros, the manuscript
#' writes \oanum{F1Detailed} and prints what this render resolved. One macro taking a key, rather than one macro per
#' number, because a LaTeX command name cannot contain a digit and half the keys do. A key the file does not hold --
#' a typo, or a number that did not resolve -- prints in bold as ??Key, so it is seen in the PDF rather than lost.
#'
#' @param .tab Tibble from oa_numbers_set().
#' @return Character: the lines of the file.
oa_numbers_tex <- function(.tab) {
  if (FALSE) .tab <- oa_state_get(.what = "Numbers")
  ok_  <- .tab[.tab$Status == "ok", , drop = FALSE]
  bad_ <- .tab$Key[.tab$Status != "ok"]
  c(
    paste0("% ", strrep("=", 118)),
    "% GENERATED FROM 1_code/31-OnlineAppendix.qmd -- DO NOT EDIT THIS FILE. Every number the online appendix cites.",
    "% Input it in the preamble, then write \\oanum{Key} anywhere in the paper; an unknown key prints as ??Key in bold.",
    paste0("% ", strrep("=", 118)),
    if (length(bad_) > 0L) {
      paste0("% Unresolved at this render, and so printed as ??Key: ", paste(bad_, collapse = ", "), ".")
    },
    "\\makeatletter",
    "\\providecommand{\\oanum}[1]{\\ifcsname oanum@#1\\endcsname\\csname oanum@#1\\endcsname\\else\\textbf{??#1}\\fi}",
    paste0("\\@namedef{oanum@", ok_$Key, "}{", ok_$Value, "}"),
    "\\makeatother"
  )
}

#' Write the numbers file, on every render
#'
#' Written unconditionally, unlike the pipeline's guarded writes: nothing downstream keys a cache on its timestamp,
#' and a fresh timestamp is what lets deployment tell a current file from a stale one.
#'
#' @param .tab Tibble from oa_numbers_set().
#' @param .path Character. The file written.
#' @return Invisibly, .path.
oa_numbers_write <- function(.tab, .path) {
  if (FALSE) {
    .tab  <- oa_state_get(.what = "Numbers")
    .path <- here::here("2_output", "31-OnlineAppendix", "Output", "OA-Numbers.tex")
  }
  fs::dir_create(fs::path_dir(.path))
  writeLines(
    text = oa_numbers_tex(.tab = .tab),
    con  = .path
  )
  n_ <- sum(.tab$Status == "ok")
  cli::cli_alert_success("{n_} number{?s} written as \\oanum{{}} macros to {.file {fs::path_file(.path)}}.")
  invisible(.path)
}


# 4. Floats: one table or figure, as LaTeX for Overleaf and as a card in the runbook ----------------------------------

# WHICH TABLES RUN LONGER THAN A PAGE. A long table is written as a longtable, which breaks across pages and repeats
# its header, and set without a float environment, which cannot break. Named here because the writer and the float
# must agree.
.long_tables <- c("CategoryTitles", "KeywordTerms")

#' What each registered exhibit needs on disk, and whether it is there
#'
#' A table needs Tables/<name>.tex and Notes/<name>.tex; a figure needs Figures/<name>.pdf, which the LaTeX pass
#' includes, Figures/<name>.png, which the runbook shows, and Notes/<name>.tex -- in 30's Output directory for an
#' exhibit of Source 30, in this document's own for Source 31. A static table lives in the Overleaf project and not
#' here, so it is listed and cannot be checked -- the one thing this runbook cannot see.
#'
#' The number is the one LaTeX will print -- the section letter and the exhibit's count among the tables, or the
#' figures, of its section -- which holds because the text sets exhibits in the registry's order and Validation
#' checks that it does.
#'
#' @param .floats Tibble: Section, Name, Kind, Source, Caption.
#' @param .static Tibble: Name, File, Note.
#' @param .dir_exhibits Character. 30's Output directory.
#' @param .dir_own Character. This document's own output directory.
#' @return Tibble: .floats joined to .static, plus Long, Number, Dir, Macro, Needs, nNeed, nHave, Ready and Status.
oa_float_files <- function(.floats, .static, .dir_exhibits, .dir_own) {
  if (FALSE) {
    .floats       <- tab_floats
    .static       <- tab_static
    .dir_exhibits <- here::here("2_output", "30-FinalExhibits", "Output")
    .dir_own      <- here::here("2_output", "31-OnlineAppendix", "Output")
  }
  out_ <- dplyr::left_join(
    .floats,
    .static,
    by           = dplyr::join_by(Name),
    relationship = "one-to-one"
  )
  needs_ <- purrr::map(seq_len(nrow(out_)), \(.i) {
    n_ <- out_$Name[.i]
    if (!is.na(out_$File[.i])) return(character(0))
    if (identical(out_$Kind[.i], "table")) {
      return(c(paste0("Tables/", n_, ".tex"), paste0("Notes/", n_, ".tex")))
    }
    c(paste0("Figures/", n_, ".pdf"), paste0("Figures/", n_, ".png"), paste0("Notes/", n_, ".tex"))
  })
  own_  <- out_$Source == "31"
  base_ <- dplyr::if_else(own_, as.character(.dir_own), as.character(.dir_exhibits))
  have_ <- purrr::map2_int(needs_, base_, \(.n, .b) sum(fs::file_exists(fs::path(.b, .n))))
  out_ |>
    dplyr::mutate(
      Long   = .data$Name %in% .long_tables,
      Number = paste0(.data$Section, dplyr::row_number()),
      .by    = c("Section", "Kind")
    ) |>
    dplyr::mutate(
      Dir    = base_,
      Macro  = dplyr::if_else(own_, "\\oaown", "\\oaexhibits"),
      Needs  = purrr::map_chr(needs_, \(.n) paste(.n, collapse = ", ")),
      nNeed  = lengths(needs_),
      nHave  = have_,
      Ready  = !is.na(.data$File) | .data$nHave == .data$nNeed,
      Status = dplyr::case_when(
        !is.na(.data$File)         ~ "static",
        .data$nHave == .data$nNeed ~ "ready",
        .data$nHave == 0L          ~ "not built",
        .default                   = "incomplete"
      )
    )
}

#' Build the appendix context, check it, and keep it for the emitters
#'
#' Built once, in Configuration, so a float call in the text names the exhibit and nothing else. The checks are the
#' ones a registry edit can break: a name twice, a kind that is neither table nor figure, a static row naming an
#' exhibit the registry does not list.
#'
#' @param .floats Tibble: Section, Name, Kind, Caption -- the appendix's exhibits in the order it prints them.
#' @param .static Tibble: Name, File, Note -- the exhibits read from the Overleaf project instead of from 30.
#' @param .dir_exhibits Character. 30's Output directory.
#' @param .dir_doc Character. The directory this document sits in; the html pass writes image paths relative to
#'   it, because Quarto reads a path starting with a slash as relative to the project root rather than the disk.
#' @param .fig_width Numeric. A figure's width as a share of the text width.
#' @param .strict Logical. TRUE makes a missing exhibit abort the render.
#' @param .dir_own Character. This document's own output directory, where it writes the exhibits it builds.
#' @return Invisibly, the context: Files, DirExhibits, DirOwn, DirDoc, FigWidth, Strict.
oa_context_set <- function(.floats, .static, .dir_exhibits, .dir_own, .dir_doc, .fig_width, .strict) {
  if (FALSE) {
    .floats       <- tab_floats
    .static       <- tab_static
    .dir_exhibits <- here::here("2_output", "30-FinalExhibits", "Output")
    .dir_own      <- here::here("2_output", "31-OnlineAppendix", "Output")
    .dir_doc      <- here::here("1_code")
    .fig_width    <- 0.96
    .strict       <- FALSE
  }
  miss_ <- setdiff(c("Section", "Name", "Kind", "Source", "Caption"), names(.floats))
  if (length(miss_) > 0L) cli::cli_abort("The float registry lacks {.field {miss_}}.")
  miss_ <- setdiff(c("Name", "File", "Note"), names(.static))
  if (length(miss_) > 0L) cli::cli_abort("The static table lacks {.field {miss_}}.")
  dup_ <- unique(.floats$Name[duplicated(.floats$Name)])
  if (length(dup_) > 0L) cli::cli_abort("Registered twice: {.val {dup_}}.")
  kind_ <- setdiff(unique(.floats$Kind), c("table", "figure"))
  if (length(kind_) > 0L) cli::cli_abort("Unknown kind{?s} in the registry: {.val {kind_}}.")
  src_ <- setdiff(unique(.floats$Source), c("30", "31"))
  if (length(src_) > 0L) cli::cli_abort("Unknown source{?s} in the registry: {.val {src_}}; 30 or 31.")
  stray_ <- setdiff(.static$Name, .floats$Name)
  if (length(stray_) > 0L) cli::cli_abort("Static row{?s} for exhibits not registered: {.val {stray_}}.")
  ctx_ <- list(
    Files       = oa_float_files(
      .floats       = .floats,
      .static       = dplyr::mutate(.static, File = as.character(.data$File), Note = as.character(.data$Note)),
      .dir_exhibits = .dir_exhibits,
      .dir_own      = .dir_own
    ),
    DirExhibits = .dir_exhibits,
    DirOwn      = .dir_own,
    DirDoc      = .dir_doc,
    FigWidth    = .fig_width,
    Strict      = .strict
  )
  oa_state_put(
    .what  = "Ctx",
    .value = ctx_
  )
  invisible(ctx_)
}

#' Read an exhibit's note as one line of text
#' @param .path Character. The Notes/<name>.tex file 30 wrote.
#' @return Character scalar.
oa_note_text <- function(.path) {
  if (FALSE) .path <- here::here("2_output", "30-FinalExhibits", "Output", "Notes", "ClassTransformer.tex")
  txt_ <- readLines(
    con  = .path,
    warn = FALSE
  )
  trimws(paste(txt_, collapse = " "))
}

#' The LaTeX of one float
#'
#' The environment carries the caption above the exhibit, as the appendix always has, the label, the exhibit read
#' through \\oaexhibits or \\oastatic, and the note below it. A table from 30 is input as the manuscript inputs
#' it: 30's central writer, fin_tex_write(), gives every table its frame -- rules, row spacing, column widths, and a
#' resize beyond eight number columns -- but no type size, which the float sets as the paper's table environment
#' does, with \\footnotesize around the \\input; nothing else is wrapped around it. A static table goes through
#' \\oafit, which shrinks one wider than the line and never enlarges one. An exhibit 30 has not built is set as
#' \\oamissing, so the paper still compiles. The macros are defined in the fragment's template and can be redefined
#' in Overleaf.
#'
#' @param .row One-row tibble from the context's Files.
#' @param .fig_width Numeric. A figure's width as a share of the text width.
#' @return Character: the lines of a raw LaTeX block.
oa_float_tex <- function(.row, .fig_width) {
  if (FALSE) {
    .row       <- oa_state_get(.what = "Ctx")$Files[1L, ]
    .fig_width <- 0.96
  }
  env_   <- if (identical(.row$Kind, "table")) "table" else "figure"
  long_  <- isTRUE(.row$Long) && isTRUE(.row$Ready) && is.na(.row$File)
  label_ <- oa_label(
    .name  = .row$Name,
    .kind  = .row$Kind,
    .scope = "oa"
  )$Label
  centre_ <- function(.x) c("\\begin{center}", .x, "\\end{center}")
  body_ <- if (!is.na(.row$File)) {
    centre_(.x = paste0("\\oafit{\\input{\\oastatic ", .row$File, "}}"))
  } else if (!.row$Ready) {
    centre_(.x = paste0("\\oamissing{", oa_tex_escape(.x = .row$Name), "}"))
  } else if (identical(env_, "table")) {
    # THE TYPE SIZE IS THE FLOAT'S, AS IN THE MANUSCRIPT. 30's writer sets no size inside a table, because the paper's
    # table environment sets \footnotesize around the \input, and this float does the same. The paragraph ends after
    # the table: a tabular is a box set inline, and without the \par the note would run beside a narrow one.
    c("\\footnotesize", paste0("\\input{", .row$Macro, " Tables/", .row$Name, "}"), "\\par\\smallskip")
  } else {
    centre_(.x = paste0("\\includegraphics[width=", .fig_width, "\\textwidth]{", .row$Macro, " Figures/", .row$Name,
                        ".pdf}"))
  }
  note_ <- if (!is.na(.row$File)) {
    if (is.na(.row$Note)) character(0) else paste0("{\\footnotesize Notes: ", oa_tex_escape(.x = .row$Note), "\\par}")
  } else if (.row$Ready) {
    paste0("{\\footnotesize Notes: \\input{", .row$Macro, " Notes/", .row$Name, "}\\par}")
  } else {
    character(0)
  }
  if (long_) {
    # A LONGTABLE CANNOT SIT IN A FLOAT, which does not break across pages. The caption is set as a paragraph above
    # it, numbered by the table counter, so the reference still reads Table C2 and the numbering keeps its order.
    return(c(
      "",
      "```{=latex}",
      "\\begin{center}",
      "\\refstepcounter{table}",
      paste0("\\textbf{Table \\thetable:} ", oa_tex_escape(.x = .row$Caption), "\\label{", label_, "}"),
      "\\end{center}",
      body_,
      note_,
      "```",
      ""
    ))
  }
  c(
    "",
    "```{=latex}",
    paste0("\\begin{", env_, "}[htbp]"),
    paste0("\\caption{", oa_tex_escape(.x = .row$Caption), "}\\label{", label_, "}"),
    body_,
    note_,
    paste0("\\end{", env_, "}"),
    "```",
    ""
  )
}

#' Escape text for raw html
#' @param .x Character.
#' @return Character, with the ampersand and the angle brackets escaped.
oa_html_escape <- function(.x) {
  if (FALSE) .x <- "R&D in <5% of contracts"
  out_ <- gsub(
    pattern     = "&",
    replacement = "&amp;",
    x           = .x,
    fixed       = TRUE
  )
  out_ <- gsub(
    pattern     = "<",
    replacement = "&lt;",
    x           = out_,
    fixed       = TRUE
  )
  out_ <- gsub(
    pattern     = ">",
    replacement = "&gt;",
    x           = out_,
    fixed       = TRUE
  )
  oa_html_ascii(.x = out_)
}

#' Every character outside ASCII as a numeric html entity
#'
#' LOCALE-PROOF OUTPUT. Pandoc writes UTF-8 -- an en dash for --, curly quotes, an accented name in a table -- and a
#' session whose native encoding is not UTF-8 mangles each of those on the way out, so the page shows the literal
#' text <U+2013> where a dash belongs. An entity is plain ASCII and survives any locale, and a browser renders it as
#' the character it names.
#'
#' @param .x Character.
#' @return Character, with every non-ASCII character replaced by &#<code>;.
oa_html_ascii <- function(.x) {
  if (FALSE) .x <- "Feb 2008 \u2013 Mar 2009"
  purrr::map_chr(.x, \(.s) {
    if (is.na(.s) || !any(utf8ToInt(.s) > 127L, na.rm = TRUE)) return(.s)
    chars_ <- strsplit(.s, "", fixed = TRUE)[[1L]]
    codes_ <- purrr::map_int(chars_, \(.c) utf8ToInt(.c)[[1L]])
    paste(dplyr::if_else(codes_ > 127L, paste0("&#", codes_, ";"), chars_), collapse = "")
  })
}

#' A framed line of text, standing in for an exhibit the runbook cannot show
#' @param .html Character. The line, already valid html.
#' @return Character scalar.
oa_html_box <- function(.html) {
  if (FALSE) .html <- "Not built yet."
  paste0(
    "<p style=\"border: 1px dashed #999; padding: 0.6em 1em; text-align: center; font-size: 0.9em;\">",
    .html,
    "</p>"
  )
}

#' Find the Pandoc this render runs
#'
#' Quarto exports its own bin directory to the R process it starts, and its Pandoc sits under tools/ there. That copy
#' is taken first, so a table is converted by the same Pandoc that writes the page around it; rmarkdown's copy and the
#' PATH are the fallbacks for a session outside a render.
#'
#' @return Character scalar: the path of a Pandoc binary, or NA where none is found.
oa_pandoc_bin <- function() {
  bin_  <- Sys.getenv("QUARTO_BIN_PATH")
  arch_ <- if (grepl("aarch64|arm64", R.version$arch)) "aarch64" else "x86_64"
  cands_ <- character(0)
  if (nzchar(bin_)) {
    cands_ <- c(
      file.path(bin_, "tools", arch_, "pandoc"),
      file.path(bin_, "tools", "pandoc"),
      Sys.glob(file.path(bin_, "tools", "*", "pandoc"))
    )
  }
  rmd_ <- tryCatch(
    if (isTRUE(rmarkdown::pandoc_available())) rmarkdown::pandoc_exec() else NA_character_,
    error = function(.e) NA_character_
  )
  cands_ <- c(cands_, rmd_, unname(Sys.which("pandoc")))
  cands_ <- cands_[!is.na(cands_) & nzchar(cands_)]
  cands_ <- cands_[fs::file_exists(cands_)]
  if (length(cands_) == 0L) return(NA_character_)
  cands_[[1L]]
}

#' The Pandoc binary, found once per render
#' @return Character scalar, or NA.
oa_pandoc_get <- function() {
  bin_ <- oa_state_get(.what = "Pandoc")
  if (!is.null(bin_)) return(bin_)
  bin_ <- oa_pandoc_bin()
  oa_state_put(
    .what  = "Pandoc",
    .value = bin_
  )
  bin_
}

#' Normalise 30's tabular LaTeX for Pandoc's reader
#'
#' Several constructs in 30's tables are misread by Pandoc, each in a way that looks plausible rather than broken, and
#' each is rewritten here before conversion. None of them carries content the html needs in that form:
#'
#' - fin_tex_write()'s current frame: the \\resizebox that wraps a table beyond eight number columns; column specs
#'   that set widths with \\dimexpr or align with >{\\raggedleft\\arraybackslash}, which Pandoc cannot read and
#'   answers by dropping cells, and which become plain l, r and c; and \\hline\\hline on top and \\hline at the
#'   bottom, which become \\toprule and \\bottomrule, since the header count and the content test key on them.
#' - fin_tex_write()'s earlier frame: the adjustbox environment, which Pandoc prints as a stray "max width=" line above the
#'   table; tabular* with its width and \extracolsep{\fill}, which becomes a plain tabular; and the \arraystretch
#'   setting. The frame sizes the table on the page, which the html does by its own means.
#' - grouping and size commands (\begingroup, \small): no content; removed.
#' - \cmidrule(lr){a-b}: Pandoc reads the option and the range as the text of the next cell; removed.
#' - \addlinespace: Pandoc empties the row that follows it; removed.
#' - \rotatebox{90}{text}: Pandoc drops the whole row the command sits in; the text is kept, unrotated.
#' - \hspace{1em}: Pandoc drops it, and with it the indentation of a sub-category; a token carries it through and
#'   becomes an em space after conversion.
#'
#' @param .lines Character. The lines of one tabular file.
#' @return Character, the same lines normalised.
oa_tex_normalise <- function(.lines) {
  if (FALSE) .lines <- c("\\begingroup\\small", "\\hspace{1em}Credit & 1 \\\\", "\\cmidrule(lr){2-3}")
  rules_ <- c(
    # A LONGTABLE'S REPEATED HEADER IS FOR THE PAGE, NOT FOR THE PAGE'S READER. Pandoc keeps the block between
    # \\endfirsthead and \\endhead and sets it as data, so the html shows the header twice and an empty row where the
    # footer rule was. The machinery is removed and the environment becomes a plain tabular, which the html scrolls.
    # The footer's own rule is kept: it is the table's bottom rule, and the content check reads between the rules.
    "(?s)\\\\endfirsthead.*?\\\\endhead"                                   = "",
    "\\\\end(first)?(head|foot)|\\\\endlastfoot"                            = "",
    "\\\\begin\\{longtable\\}"                                          = "\\\\begin{tabular}",
    "\\\\end\\{longtable\\}"                                            = "\\\\end{tabular}",
    "\\\\resizebox\\{[^}]*\\}\\{!\\}\\{"                                   = "",
    "\\\\end\\{tabular\\}\\s*\\}"                                         = "\\\\end{tabular}",
    ">\\{\\\\raggedleft\\\\arraybackslash\\}p\\{[^}]*\\}"                  = "r",
    ">\\{\\\\centering\\\\arraybackslash\\}p\\{[^}]*\\}"                   = "c",
    ">\\{\\\\raggedright\\\\arraybackslash\\}p\\{[^}]*\\}"                 = "l",
    "p\\{\\\\dimexpr[^}]*\\}"                                               = "l",
    "\\\\hline\\s*\\\\hline"                                               = "\\\\toprule",
    "\\\\hline"                                                            = "\\\\bottomrule",
    "\\\\(begin|end)\\{adjustbox\\}(\\{[^}]*\\})?"                        = "",
    "\\\\begin\\{tabular\\*\\}\\{[^}]*\\}\\{@\\{\\\\extracolsep\\{\\\\fill\\}\\}\\s*" = "\\\\begin{tabular}{",
    "\\\\end\\{tabular\\*\\}"                                            = "\\\\end{tabular}",
    "\\\\renewcommand\\{\\\\arraystretch\\}\\{[^}]*\\}"                   = "",
    "\\\\begingroup|\\\\endgroup|\\\\(small|footnotesize|scriptsize)\\b" = "",
    "\\\\cmidrule(\\([a-z]*\\))?\\{[0-9]+-[0-9]+\\}"                    = "",
    "\\\\addlinespace(\\[[^]]*\\])?"                                     = "",
    "\\\\rotatebox\\{[^}]*\\}\\{([^}]*)\\}"                              = "\\1",
    "\\\\hspace\\{1em\\}"                                                = "OAINDENT"
  )
  # ONE STRING, NOT LINES: the \\resizebox wrapper closes on a line of its own after \\end{tabular}.
  out_ <- paste(.lines, collapse = "\n")
  for (k_ in names(rules_)) {
    out_ <- gsub(
      pattern     = k_,
      replacement = rules_[[k_]],
      x           = out_,
      perl        = TRUE
    )
  }
  strsplit(out_, split = "\n", fixed = TRUE)[[1L]]
}

#' How many header rows a tabular has: the rows between \\toprule and the first \\midrule
#' @param .lines Character. The lines of one normalised tabular file.
#' @return Integer; 0 where either rule is absent.
oa_tex_header_rows <- function(.lines) {
  if (FALSE) .lines <- c("\\toprule", "A & B \\\\", "a & b \\\\", "\\midrule", "1 & 2 \\\\")
  txt_ <- paste(.lines, collapse = "\n")
  top_ <- stringi::stri_locate_first_fixed(txt_, "\\toprule")[, "end"]
  mid_ <- stringi::stri_locate_first_fixed(txt_, "\\midrule")[, "start"]
  if (is.na(top_) || is.na(mid_) || mid_ < top_) return(0L)
  seg_ <- stringi::stri_sub(
    str  = txt_,
    from = top_ + 1L,
    to   = mid_ - 1L
  )
  as.integer(stringi::stri_count_fixed(seg_, "\\\\"))
}

#' Move a table's header rows into a thead
#'
#' Pandoc's LaTeX reader recognises one header row. 30's tables with a spanning header have two, and Pandoc then
#' sets both in the body, where they read as data. The count comes from the LaTeX, where the header is unambiguous.
#'
#' @param .html Character scalar. Pandoc's html for one table.
#' @param .k Integer. Header rows, from oa_tex_header_rows().
#' @return Character scalar.
oa_html_promote_header <- function(.html, .k) {
  if (FALSE) {
    .html <- "<table><tbody><tr><td>A</td></tr><tr><td>1</td></tr></tbody></table>"
    .k    <- 1L
  }
  if (.k < 1L || grepl("<thead>", .html, fixed = TRUE)) return(.html)
  rows_ <- stringi::stri_extract_all_regex(.html, "(?s)<tr[^>]*>.*?</tr>")[[1L]]
  if (length(rows_) <= .k) return(.html)
  head_ <- gsub(
    pattern     = "<td",
    replacement = "<th",
    x           = gsub("</td>", "</th>", rows_[seq_len(.k)], fixed = TRUE),
    fixed       = TRUE
  )
  cols_ <- stringi::stri_extract_first_regex(.html, "(?s)<colgroup>.*?</colgroup>")
  paste0(
    "<table>",
    if (is.na(cols_)) "" else cols_,
    "<thead>", paste(head_, collapse = ""), "</thead>",
    "<tbody>", paste(rows_[-seq_len(.k)], collapse = ""), "</tbody>",
    "</table>"
  )
}

#' Which words and numbers of a table did not survive its conversion
#'
#' The content test behind the residue flag. Every word and number in the LaTeX cells, command names set aside and
#' their arguments kept, must appear in the html at least as often: Pandoc meeting a construct it does not know does
#' not fail, it drops the cell's text or splits the row, and only a comparison of content sees that. Tokens are runs
#' of letters and digits, so 1,375 is 1 and 375 on both sides and punctuation cannot cause a false alarm.
#'
#' @param .src Character. The normalised LaTeX lines the html was converted from.
#' @param .html Character scalar. The converted table.
#' @return Character: the tokens missing from the html, empty where the conversion kept everything.
oa_table_lost <- function(.src, .html) {
  if (FALSE) {
    .src  <- c("\\toprule", "A & B \\\\", "\\midrule", "\\makecell{Industry\\\\code} & 1 \\\\", "\\bottomrule")
    .html <- "<table><tr><td>code</td><td>1</td></tr></table>"
  }
  txt_ <- paste(.src, collapse = " ")
  top_ <- stringi::stri_locate_first_fixed(txt_, "\\toprule")[, "end"]
  bot_ <- stringi::stri_locate_last_fixed(txt_, "\\bottomrule")[, "start"]
  if (!is.na(top_) && !is.na(bot_) && bot_ > top_) {
    txt_ <- stringi::stri_sub(
      str  = txt_,
      from = top_ + 1L,
      to   = bot_ - 1L
    )
  }
  clean_ <- c(
    "\\\\multicolumn\\{[0-9]+\\}\\{[^}]*\\}" = " ",
    "\\\\[%&$#_{}]"                         = " ",
    "\\\\[A-Za-z]+\\*?"                     = " ",
    "OAINDENT"                              = " "
  )
  for (k_ in names(clean_)) {
    txt_ <- gsub(
      pattern     = k_,
      replacement = clean_[[k_]],
      x           = txt_,
      perl        = TRUE
    )
  }
  out_ <- gsub(
    pattern     = "<[^>]+>|&[a-z]+;",
    replacement = " ",
    x           = .html,
    perl        = TRUE
  )
  src_  <- table(stringi::stri_extract_all_regex(txt_, "[[:alnum:]]+")[[1L]])
  html_ <- table(stringi::stri_extract_all_regex(out_, "[[:alnum:]]+")[[1L]])
  have_ <- as.integer(html_[names(src_)])
  names(src_)[is.na(have_) | have_ < as.integer(src_)]
}

#' One of 30's tabulars as html
#'
#' The file the paper inputs is the file converted here, so the runbook shows the paper's own table rather than a
#' second rendering of the data behind it. A conversion that fails returns NA, and the caller says so in the page.
#'
#' RESIDUE AND LOSS ARE REPORTED, NOT HIDDEN. The normalisations above were written against the constructs 30 uses
#' today; one added to 30's writer later reaches Pandoc unnormalised, and Pandoc does not fail on it. It either prints
#' the command's leftovers beside the table -- as fin_tex_write()'s frame did before its three rules existed -- or it
#' drops a cell's text, or splits a row at a line break inside a cell. Text outside the table and spans catch the
#' first; oa_table_lost() catches the other two by content. Either marks the table, and Validation names it.
#'
#' @param .path_tex Character. Tables/<name>.tex, as 30 wrote it.
#' @param .pandoc Character. The Pandoc binary, or NA.
#' @return List: Html (character scalar, NA where the conversion failed), Residue (logical) and Lost (the tokens of the
#'   LaTeX missing from the html).
oa_table_html <- function(.path_tex, .pandoc) {
  if (FALSE) {
    .path_tex <- here::here("2_output", "30-FinalExhibits", "Output", "Tables", "ClassTransformer.tex")
    .pandoc   <- oa_pandoc_bin()
  }
  fail_ <- list(
    Html    = NA_character_,
    Residue = FALSE,
    Lost    = character(0)
  )
  if (is.na(.pandoc)) return(fail_)
  src_ <- oa_tex_normalise(.lines = readLines(con = .path_tex, warn = FALSE))
  k_   <- oa_tex_header_rows(.lines = src_)
  in_  <- tempfile(fileext = ".tex")
  out_ <- tempfile(fileext = ".html")
  on.exit(unlink(c(in_, out_)), add = TRUE)
  writeLines(
    text = src_,
    con  = in_
  )
  res_ <- suppressWarnings(system2(
    command = .pandoc,
    args    = c("-f", "latex", "-t", "html", "--wrap=none", "-o", shQuote(out_), shQuote(in_)),
    stdout  = TRUE,
    stderr  = TRUE
  ))
  if (!is.null(attr(res_, "status")) || !fs::file_exists(out_)) return(fail_)
  # UTF-8 EXPLICITLY. Pandoc writes UTF-8 -- an en dash for --, curly quotes, an accented name in a table -- and a
  # session whose native encoding is not UTF-8 turns each of those into the literal text <U+2013>, which the page
  # then shows as an unknown tag. Reading through a declared connection keeps them characters.
  con_ <- file(out_, encoding = "UTF-8")
  on.exit(close(con_), add = TRUE)
  html_ <- paste(readLines(con = con_, warn = FALSE), collapse = "\n")
  if (!grepl("<table", html_, fixed = TRUE)) return(fail_)
  outside_ <- stringi::stri_replace_all_regex(html_, "(?s)<table.*?</table>", "")
  # A MATH SPAN IS CONTENT: Pandoc sets $\\geq$ as <span class="math inline">, which is the symbol, not a leftover.
  residue_ <- nzchar(trimws(gsub("<[^>]+>", "", outside_))) || grepl("<span(?! class=\"math)", html_, perl = TRUE)
  html_ <- stringi::stri_extract_first_regex(html_, "(?s)<table.*?</table>")
  html_ <- oa_html_promote_header(
    .html = html_,
    .k    = k_
  )
  # A NUMBER AND ITS SPREAD STAY ON ONE LINE. Right-aligned cells hold figures such as 0.981 (0.004), which a narrow
  # page would break in two; the table scrolls sideways instead. Text columns may still wrap.
  html_ <- gsub(
    pattern     = "style=\"text-align: right;\"",
    replacement = "style=\"text-align: right; white-space: nowrap;\"",
    x           = html_,
    fixed       = TRUE
  )
  html_ <- gsub(
    pattern     = "OAINDENT",
    replacement = "&emsp;",
    x           = html_,
    fixed       = TRUE
  )
  lost_ <- oa_table_lost(
    .src  = src_,
    .html = html_
  )
  list(
    Html    = oa_html_ascii(.x = html_),
    Residue = residue_ || length(lost_) > 0L,
    Lost    = lost_
  )
}

#' The html pass's version of one float: the exhibit in place, numbered as the paper numbers it
#'
#' A figure is 30's png copy; a table is 30's own tabular converted by oa_table_html(). Caption above and note below,
#' as the paper sets them. What the runbook cannot show -- a static table, which lives in the Overleaf project, and an
#' exhibit 30 has not built -- is a framed line saying so. The block is raw html, so a note's brackets and asterisks
#' reach the page as written rather than as markdown.
#'
#' @param .row One-row tibble from the context's Files.
#' @param .ctx List. The appendix context: DirDoc is used; each exhibit's own directory comes with its row.
#' @return List: Lines (a raw html block), Shown (TRUE where the exhibit itself is on the page) and Residue
#'   (NA where the conversion was faithful; otherwise a short description with the first tokens it lost).
oa_float_md <- function(.row, .ctx) {
  if (FALSE) {
    .ctx <- oa_state_get(.what = "Ctx")
    .row <- .ctx$Files[1L, ]
  }
  anchor_ <- oa_label(
    .name  = .row$Name,
    .kind  = .row$Kind,
    .scope = "oa"
  )$Anchor
  word_  <- if (identical(.row$Kind, "table")) "Table" else "Figure"
  head_  <- paste0("<p><strong>", word_, " ", .row$Number, ":</strong> ", oa_html_escape(.x = .row$Caption), "</p>")
  note_  <- if (!is.na(.row$File)) {
    .row$Note
  } else if (.row$Ready) {
    oa_note_text(.path = fs::path(.row$Dir, "Notes", paste0(.row$Name, ".tex")))
  } else {
    NA_character_
  }
  shown_   <- FALSE
  residue_ <- FALSE
  lost_    <- character(0)
  body_ <- if (!is.na(.row$File)) {
    oa_html_box(.html = paste0(
      "Set from <code>", oa_html_escape(.x = .row$File), ".tex</code> among the Overleaf project's static tables, ",
      "which this document cannot read."
    ))
  } else if (!.row$Ready) {
    why_ <- unname(oa_state_get(.what = "Builds")[.row$Name])
    oa_html_box(.html = if (identical(.row$Status, "incomplete")) {
      writer_ <- if (identical(.row$Source, "31")) "this appendix has built " else "30 has written "
      paste0("Incomplete: ", writer_, .row$nHave, " of the ", .row$nNeed, " files it needs (",
             oa_html_escape(.x = .row$Needs), ").")
    } else if (!identical(.row$Source, "31")) {
      paste0("Not built yet: 30 would write ", oa_html_escape(.x = .row$Needs), ".")
    } else if (length(why_) == 1L && !is.na(why_) && grepl("^waiting", why_)) {
      paste0("Not built yet: this appendix builds it, and its build is ", oa_html_escape(.x = why_), ".")
    } else {
      "Not built yet: this appendix builds it, and its builder is still to be written."
    })
  } else if (identical(.row$Kind, "figure")) {
    shown_ <- TRUE
    png_ <- fs::path_rel(
      path  = fs::path(.row$Dir, "Figures", paste0(.row$Name, ".png")),
      start = .ctx$DirDoc
    )
    paste0("<img src=\"", png_, "\" alt=\"", oa_html_escape(.x = .row$Caption), "\" style=\"width: 100%;\">")
  } else {
    tab_ <- oa_table_html(
      .path_tex = fs::path(.row$Dir, "Tables", paste0(.row$Name, ".tex")),
      .pandoc   = oa_pandoc_get()
    )
    residue_ <- tab_$Residue
    lost_    <- tab_$Lost
    if (is.na(tab_$Html)) {
      oa_html_box(.html = paste0("Could not be converted here; the paper sets it from Tables/", .row$Name, ".tex."))
    } else {
      shown_ <- TRUE
      paste0("<div style=\"overflow-x: auto;\">\n", tab_$Html, "\n</div>")
    }
  }
  foot_ <- if (is.na(note_)) character(0) else {
    paste0("<p style=\"font-size: 0.85em;\"><em>Notes:</em> ", oa_html_escape(.x = note_), "</p>")
  }
  list(
    Lines = c(
      "",
      "```{=html}",
      paste0("<div id=\"", anchor_, "\" class=\"oa-float\" style=\"margin: 1.8em 0;\">"),
      head_,
      body_,
      foot_,
      "</div>",
      "```",
      ""
    ),
    Shown   = shown_,
    Residue = if (residue_) paste(c("residue", utils::head(lost_, 6L)), collapse = " ") else NA_character_
  )
}

#' Set one float in the appendix
#'
#' The emitter the text calls, in a `results: asis` chunk, wherever an exhibit belongs. It writes the LaTeX float in
#' the LaTeX pass and the exhibit itself in the html pass, records what it emitted, and refuses to emit one twice.
#'
#' @param .name Character. The exhibit's stem, as registered in Configuration.
#' @return Invisibly, the exhibit's row of the context.
oa_float <- function(.name) {
  if (FALSE) .name <- "ClassTransformer"
  ctx_ <- oa_state_get(.what = "Ctx")
  if (is.null(ctx_)) cli::cli_abort("No appendix context: oa_context_set() has not run.")
  row_ <- ctx_$Files[ctx_$Files$Name == .name, , drop = FALSE]
  if (nrow(row_) != 1L) cli::cli_abort("{.val {(.name)}} is not in the float registry.")
  done_ <- oa_state_get(.what = "Emitted")
  if (.name %in% done_) cli::cli_abort("{.val {(.name)}} is set twice; every exhibit appears once.")
  if (isTRUE(ctx_$Strict) && !row_$Ready) {
    cli::cli_abort("Strict: {.val {(.name)}} is {row_$Status}; 30 writes {row_$Needs}.")
  }
  oa_state_put(
    .what  = "Emitted",
    .value = c(done_, .name)
  )
  if (oa_is_tex()) {
    lines_ <- oa_float_tex(
      .row       = row_,
      .fig_width = ctx_$FigWidth
    )
  } else {
    out_ <- oa_float_md(
      .row = row_,
      .ctx = ctx_
    )
    lines_ <- out_$Lines
    shown_ <- oa_state_get(.what = "Shown")
    shown_[[.name]] <- out_$Shown
    oa_state_put(
      .what  = "Shown",
      .value = shown_
    )
    resid_ <- oa_state_get(.what = "Residue")
    resid_[[.name]] <- out_$Residue
    oa_state_put(
      .what  = "Residue",
      .value = resid_
    )
  }
  cat(
    lines_,
    sep = "\n"
  )
  invisible(row_)
}

#' Report the float registry against disk: every exhibit, its section, and whether it can be set
#'
#' @param .files Tibble. The context's Files.
#' @return Invisibly, .files.
oa_report_floats <- function(.files) {
  if (FALSE) .files <- oa_state_get(.what = "Ctx")$Files
  show_ <- .files |>
    dplyr::transmute(
      Number  = .data$Number,
      Name    = .data$Name,
      Kind    = .data$Kind,
      Status  = .data$Status,
      Files   = dplyr::if_else(.data$Status == "static", "-", paste0(.data$nHave, " of ", .data$nNeed))
    )
  tbl_say(
    .tab   = show_,
    .title = "The appendix's exhibits, in print order"
  )
  n_ <- table(.files$Status)
  cli::cli_alert_info(
    "{sum(n_)} exhibit{?s}: {paste(names(n_), n_, sep = ' ', collapse = ', ')}."
  )
  cli::cli_alert_info("Not built and incomplete are set as framed placeholders; static is read from the Overleaf project.")
  invisible(.files)
}

#' Report what the text emitted against what the registry lists, and what the html shows
#'
#' A registered exhibit the text never set is the failure this catches first: it would be listed as ready above and
#' be absent from the paper, and nothing else would say so. The second count is the html pass's: a ready exhibit that
#' is not shown in place is a table Pandoc could not convert, which the page marks where the table should be.
#'
#' @param .strict Logical. TRUE aborts where a registered exhibit was never emitted.
#' @return Invisibly, a tibble: Number, Name, Status, Emitted, Shown.
oa_report_emitted <- function(.strict) {
  if (FALSE) .strict <- FALSE
  ctx_   <- oa_state_get(.what = "Ctx")
  done_  <- oa_state_get(.what = "Emitted")
  shown_ <- oa_state_get(.what = "Shown")
  tab_ <- ctx_$Files |>
    dplyr::transmute(
      Number  = .data$Number,
      Name    = .data$Name,
      Status  = .data$Status,
      Emitted = .data$Name %in% done_,
      Shown   = .data$Name %in% names(shown_)[shown_]
    )
  order_ok_ <- identical(done_, ctx_$Files$Name[ctx_$Files$Name %in% done_])
  miss_ <- tab_$Name[!tab_$Emitted]
  if (length(miss_) == 0L) {
    cli::cli_alert_success("Every registered exhibit is set exactly once.")
  } else {
    cli::cli_alert_warning("{length(miss_)} registered exhibit{?s} never set in the text: {.val {miss_}}.")
    if (isTRUE(.strict)) cli::cli_abort("Strict: a registered exhibit is missing from the text.")
  }
  if (order_ok_) {
    cli::cli_alert_success("The text sets them in the registry's order, so the numbers above are the paper's.")
  } else {
    cli::cli_alert_warning("The text sets them out of the registry's order; the html numbers will not match the paper.")
  }
  ready_ <- tab_$Status == "ready"
  lost_  <- tab_$Name[ready_ & tab_$Emitted & !tab_$Shown]
  cli::cli_alert_info("{sum(tab_$Shown)} of {sum(ready_)} ready exhibit{?s} shown in place on this page.")
  if (length(lost_) > 0L) {
    cli::cli_alert_warning("Not converted, marked where each table should be: {.val {lost_}}.")
  }
  resid_ <- oa_state_get(.what = "Residue")
  resid_ <- resid_[!is.na(resid_)]
  if (length(resid_) > 0L) {
    cli::cli_alert_warning(
      "Converted unfaithfully -- leftover text, or words and numbers of the LaTeX missing from the page -- so 30's \\
       writer uses a construct oa_tex_normalise() does not know yet:"
    )
    cli::cli_verbatim(paste0("  ", names(resid_), ": ", resid_))
  } else if (sum(tab_$Shown) > 0L) {
    cli::cli_alert_success("Every converted table kept all of its words and numbers.")
  }
  invisible(tab_)
}


# 5. Markers: what is still to write, to decide and to verify ------------------------------------------------------------

#' Count the open markers in the text, by appendix section
#'
#' The text marks what is not yet written, not yet decided, or stated but still to be checked in square brackets
#' opening with TO WRITE, TO DECIDE or TO VERIFY, set in bold, so both renders show them. This counts them per
#' level-two heading of this document's source.
#'
#' @param .path_qmd Character. This document.
#' @return Tibble: Section, ToWrite, ToDecide, ToVerify -- sections holding at least one marker.
oa_markers <- function(.path_qmd) {
  if (FALSE) .path_qmd <- here::here("1_code", "31-OnlineAppendix.qmd")
  lines_ <- readLines(
    con  = .path_qmd,
    warn = FALSE
  )
  head_ <- stringi::stri_match_first_regex(
    str     = lines_,
    pattern = "^## +(.+?)(?: +\\{#[^}]*\\})?\\s*$"
  )[, 2L]
  sec_ <- purrr::accumulate(
    .x    = head_,
    .f    = \(.acc, .h) if (is.na(.h)) .acc else .h,
    .init = "(before the appendix)"
  )[-1L]
  tibble::tibble(
    Section  = sec_,
    ToWrite  = stringi::stri_count_fixed(lines_, "[TO WRITE"),
    ToDecide = stringi::stri_count_fixed(lines_, "[TO DECIDE"),
    ToVerify = stringi::stri_count_fixed(lines_, "[TO VERIFY")
  ) |>
    dplyr::summarise(
      ToWrite  = sum(.data$ToWrite),
      ToDecide = sum(.data$ToDecide),
      ToVerify = sum(.data$ToVerify),
      .by      = "Section"
    ) |>
    dplyr::filter(.data$ToWrite + .data$ToDecide + .data$ToVerify > 0L)
}

#' Report the open markers
#' @param .tab Tibble from oa_markers().
#' @return Invisibly, .tab.
oa_report_markers <- function(.tab) {
  if (FALSE) .tab <- oa_markers(.path_qmd = here::here("1_code", "31-OnlineAppendix.qmd"))
  if (nrow(.tab) == 0L) {
    cli::cli_alert_success("No open markers: nothing is marked as still to write, to decide or to verify.")
    return(invisible(.tab))
  }
  tbl_say(
    .tab   = .tab,
    .title = "Open markers, by appendix section"
  )
  cli::cli_alert_info(
    "{sum(.tab$ToWrite)} passage{?s} to write, {sum(.tab$ToDecide)} decision{?s} open, \\
     {sum(.tab$ToVerify)} fact{?s} to verify."
  )
  invisible(.tab)
}


# 6. Inputs --------------------------------------------------------------------------------------------------------------

#' Report the inputs: 30's output directories and the two files the renders need
#'
#' 30's output directory is the one input without which nothing here means anything, so its absence aborts; an empty
#' Data/ does not, since every number then resolves to a marker and the report says so.
#'
#' @param .dir_exhibits Character. 30's Output directory.
#' @param .template Character. The LaTeX pass's template.
#' @param .bib Character. The html pass's bibliography.
#' @return Invisibly, a tibble: Input, Path, Exists, Files.
oa_report_inputs <- function(.dir_exhibits, .template, .bib) {
  if (FALSE) {
    .dir_exhibits <- here::here("2_output", "30-FinalExhibits", "Output")
    .template     <- here::here("1_code", "_Templates", "oa-body.tex")
    .bib          <- here::here("1_code", "_Templates", "oa-references.bib")
  }
  if (!fs::dir_exists(.dir_exhibits)) {
    cli::cli_abort(c(
      "30's output directory does not exist.",
      "x" = "{.file {(.dir_exhibits)}}",
      "i" = "Render 30-FinalExhibits first; this document only reads what 30 wrote."
    ))
  }
  subs_  <- c("Data", "Tables", "Notes", "Figures")
  dirs_  <- fs::path(.dir_exhibits, subs_)
  count_ <- purrr::map_int(dirs_, \(.d) if (fs::dir_exists(.d)) length(fs::dir_ls(.d, type = "file")) else 0L)
  tab_ <- tibble::tibble(
    Input  = c(paste0("30 ", subs_), "Template", "Bibliography"),
    Path   = as.character(fs::path_rel(c(dirs_, .template, .bib), start = here::here())),
    Exists = c(fs::dir_exists(dirs_), fs::file_exists(c(.template, .bib))),
    Files  = c(count_, NA_integer_, NA_integer_)
  )
  tbl_say(
    .tab   = tab_,
    .title = "What the appendix reads"
  )
  if (!all(tab_$Exists[tab_$Input %in% c("Template", "Bibliography")])) {
    cli::cli_abort("The template or the bibliography is missing; see the table above.")
  }
  cli::cli_alert_info("Zero files under 30 Data means every number becomes a marker; the Numbers report lists them.")
  invisible(tab_)
}


# 7. Rendering and deployment: the LaTeX pass, and the fragment to the paper folder ----------------------------------
# THE RENDER BUTTON RENDERS ONE FORMAT. RStudio renders the first format the YAML lists, which is the html runbook, so
# a button render never writes the fragment. A terminal render, quarto render without --to, runs both passes, html
# first. From the console, oa_render_tex() runs the LaTeX pass on its own and oa_deploy_tex() copies the result.

#' Which of the fragment's inputs are newer than the fragment
#'
#' A fragment is stale when anything it was rendered from has changed since: this document, its library, or what 30
#' wrote -- a number moves with Data/, and an exhibit built since turns a placeholder into a float. The newest file
#' under 30's four exhibit folders stands for 30; Prepared/ is left out, because nothing in it reaches the fragment.
#'
#' @param .path_tex Character. The fragment.
#' @param .path_qmd Character. This document; its library is the .R file of the same stem.
#' @param .dir_exhibits Character. 30's Output directory.
#' @return Tibble: Source, Modified, Newer -- TRUE where the source postdates the fragment, NA where either is absent.
oa_tex_status <- function(.path_tex, .path_qmd, .dir_exhibits) {
  if (FALSE) {
    .path_tex     <- here::here("_rendered", "1_code", "31-OnlineAppendix.tex")
    .path_qmd     <- here::here("1_code", "31-OnlineAppendix.qmd")
    .dir_exhibits <- here::here("2_output", "30-FinalExhibits", "Output")
  }
  dirs_  <- fs::path(.dir_exhibits, c("Data", "Tables", "Notes", "Figures"))
  files_ <- unlist(lapply(dirs_[fs::dir_exists(dirs_)], \(.d) fs::dir_ls(.d, type = "file")))
  newest_ <- if (length(files_) == 0L) as.POSIXct(NA) else max(fs::file_info(files_)$modification_time)
  tex_ <- if (fs::file_exists(.path_tex)) fs::file_info(.path_tex)$modification_time else as.POSIXct(NA)
  tibble::tibble(
    Source   = c("this document", "its library", "30's exhibits"),
    Modified = c(
      fs::file_info(.path_qmd)$modification_time,
      fs::file_info(fs::path_ext_set(.path_qmd, "R"))$modification_time,
      newest_
    )
  ) |>
    dplyr::mutate(Newer = .data$Modified > tex_)
}

#' Report the fragment on disk: whether it exists, and whether anything it was rendered from is newer
#'
#' During a render this sees the fragment as it stood before the render: a terminal render replaces it after the
#' html pass, and a Render-button render leaves it as it is. The message says which to run when the file is missing.
#'
#' @param .path_tex Character. The fragment the LaTeX pass writes.
#' @param .path_qmd Character. This document.
#' @param .dir_exhibits Character. 30's Output directory.
#' @return Invisibly, the status tibble from oa_tex_status(), or NULL where there is no fragment.
oa_report_tex <- function(.path_tex, .path_qmd, .dir_exhibits) {
  if (FALSE) {
    .path_tex     <- here::here("_rendered", "1_code", "31-OnlineAppendix.tex")
    .path_qmd     <- here::here("1_code", "31-OnlineAppendix.qmd")
    .dir_exhibits <- here::here("2_output", "30-FinalExhibits", "Output")
  }
  if (!fs::file_exists(.path_tex)) {
    cli::cli_alert_warning(
      "No fragment on disk. Only the LaTeX pass writes {.file {fs::path_file(.path_tex)}}, and the Render button \\
       does not run it."
    )
    cli::cli_alert_info(
      "From the console: {.code oa_render_tex()}, as shown below. From the terminal: \\
       {.code quarto render 1_code/31-OnlineAppendix.qmd}, which runs both passes."
    )
    return(invisible(NULL))
  }
  st_ <- oa_tex_status(
    .path_tex     = .path_tex,
    .path_qmd     = .path_qmd,
    .dir_exhibits = .dir_exhibits
  )
  tex_ <- fs::file_info(.path_tex)$modification_time
  show_ <- dplyr::bind_rows(
    tibble::tibble(Source = "the fragment", Modified = tex_, Newer = NA),
    st_
  ) |>
    dplyr::transmute(
      Source   = .data$Source,
      Modified = format(.data$Modified, "%Y-%m-%d %H:%M:%S"),
      Newer    = dplyr::case_when(
        is.na(.data$Newer) ~ "-",
        .data$Newer        ~ "newer: stale",
        .default           = "older"
      )
    )
  tbl_say(
    .tab   = show_,
    .title = "The fragment against what it was rendered from"
  )
  stale_ <- st_$Source[dplyr::coalesce(st_$Newer, FALSE)]
  if (length(stale_) > 0L) {
    cli::cli_alert_warning("Stale: {stale_} changed after the fragment was written. Run the LaTeX pass before deploying.")
  } else {
    cli::cli_alert_success("The fragment is newer than everything it was rendered from.")
  }
  invisible(st_)
}

#' Find the Quarto binary a terminal would use
#'
#' @param .quarto Character or NULL. An explicit path, tried first.
#' @return Character scalar: the path of the Quarto binary.
oa_quarto_bin <- function(.quarto) {
  if (FALSE) .quarto <- NULL
  cands_ <- c(
    .quarto,
    Sys.getenv("QUARTO_PATH"),
    unname(Sys.which("quarto")),
    "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/quarto" # the copy RStudio bundles on macOS
  )
  cands_ <- cands_[!is.na(cands_) & nzchar(cands_)]
  hit_ <- cands_[fs::file_exists(cands_)]
  if (length(hit_) == 0L) {
    cli::cli_abort(c(
      "No Quarto binary found from this R session.",
      "i" = "Pass it as {.arg .quarto}, or run \\
             {.code quarto render 1_code/31-OnlineAppendix.qmd --to latex} in the terminal."
    ))
  }
  hit_[[1L]]
}

#' Run the LaTeX pass on its own, from the console
#'
#' The Render button renders the html runbook and nothing else, so the fragment needs a pass of its own. This runs
#' it through Quarto in a separate R process, exactly as a terminal render does, so nothing in this session is
#' touched; Quarto's output streams to the console. It then checks that the fragment was actually written by this
#' call rather than left over from an earlier one.
#'
#' @param .path_qmd Character. This document.
#' @param .path_tex Character. The fragment the LaTeX pass writes.
#' @param .quarto Character or NULL. The Quarto binary; NULL tries QUARTO_PATH, the PATH, then RStudio's copy.
#' @return Invisibly, .path_tex.
oa_render_tex <- function(.path_qmd, .path_tex, .quarto = NULL) {
  if (FALSE) {
    .path_qmd <- here::here("1_code", "31-OnlineAppendix.qmd")
    .path_tex <- here::here("_rendered", "1_code", "31-OnlineAppendix.tex")
    .quarto   <- NULL
  }
  bin_ <- oa_quarto_bin(.quarto = .quarto)
  cli::cli_alert_info("LaTeX pass with {.file {(bin_)}}.")
  t0_ <- Sys.time() - 1
  status_ <- system2(
    command = bin_,
    args    = c("render", shQuote(.path_qmd), "--to", "latex")
  )
  if (!identical(as.integer(status_), 0L)) {
    cli::cli_abort("The LaTeX pass failed with exit status {status_}; Quarto's output above says where.")
  }
  if (!fs::file_exists(.path_tex) || fs::file_info(.path_tex)$modification_time < t0_) {
    cli::cli_abort("Quarto finished, but {.file {(.path_tex)}} was not written by this call.")
  }
  cli::cli_alert_success("Fragment written: {.file {(.path_tex)}}.")
  invisible(.path_tex)
}

#' Whether two generated files say the same thing
#'
#' The fragment's first line carries the date it was rendered, so a byte comparison would call every new day's render a
#' change and ask for an upload that changes nothing in the paper. The generated-header line is set aside; everything
#' else must match exactly. A file other than .tex -- a figure -- is compared byte for byte.
#'
#' @param .a Character. One file.
#' @param .b Character. The other.
#' @return Logical scalar.
oa_same_content <- function(.a, .b) {
  if (FALSE) {
    .a <- here::here("_rendered", "1_code", "31-OnlineAppendix.tex")
    .b <- .a
  }
  if (!grepl("[.]tex$", .a)) return(identical(unname(tools::md5sum(.a)), unname(tools::md5sum(.b))))
  body_ <- function(.p) {
    x_ <- readLines(
      con  = .p,
      warn = FALSE
    )
    x_[!grepl("^% GENERATED FROM", x_)]
  }
  identical(body_(.a), body_(.b))
}

#' Copy the fragment, the numbers file and this appendix's own exhibits to the paper folder, and say which to upload
#'
#' Run from the console, or by oa_publish(). The folder is this document's own, beside 30's current/ rather than
#' inside it: 30's deployment moves current/ to its archive wholesale and would take these files with it. The
#' exhibits this appendix builds itself travel with the fragment, in the same Tables/, Notes/ and Figures/ layout,
#' since the fragment reads them through \oaown from the same folder in Overleaf. A stale fragment or numbers file is
#' refused, because deploying one publishes an appendix that no longer matches its source. A file identical to the
#' deployed one, its generated header aside, is not copied, and a replaced one is archived; what remains is the list
#' of files that changed, which is exactly what has to be uploaded to Overleaf by hand.
#'
#' @param .path_tex Character. The fragment the LaTeX pass wrote.
#' @param .path_numbers Character. The numbers file the render wrote.
#' @param .path_qmd Character. This document, to test the fragment and the numbers file for staleness.
#' @param .dir_exhibits Character. 30's Output directory, to test both files for staleness.
#' @param .dir_own Character. This document's own output directory, whose Tables/, Notes/ and Figures/ travel along.
#' @param .dir_deploy Character. The paper folder's OnlineAppendix directory.
#' @return Invisibly, the paths (relative to the folder) of the files that changed.
oa_deploy_tex <- function(.path_tex, .path_numbers, .path_qmd, .dir_exhibits, .dir_own, .dir_deploy) {
  if (FALSE) {
    .path_tex     <- here::here("_rendered", "1_code", "31-OnlineAppendix.tex")
    .path_numbers <- here::here("2_output", "31-OnlineAppendix", "Output", "OA-Numbers.tex")
    .path_qmd     <- here::here("1_code", "31-OnlineAppendix.qmd")
    .dir_exhibits <- here::here("2_output", "30-FinalExhibits", "Output")
    .dir_own      <- here::here("2_output", "31-OnlineAppendix", "Output")
    .dir_deploy   <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "200-Paper_figures",
      "OnlineAppendix"
    )
  }
  paths_ <- c(.path_tex, .path_numbers)
  miss_  <- paths_[!fs::file_exists(paths_)]
  if (length(miss_) > 0L) cli::cli_abort("Nothing to deploy yet: {.file {miss_}} {?does/do} not exist. Render first.")
  for (p_ in paths_) {
    st_ <- oa_tex_status(
      .path_tex     = p_,
      .path_qmd     = .path_qmd,
      .dir_exhibits = .dir_exhibits
    )
    stale_ <- st_$Source[dplyr::coalesce(st_$Newer, FALSE)]
    if (length(stale_) > 0L) {
      cli::cli_abort(c(
        "{.file {fs::path_file(p_)}} is stale: {stale_} changed after it was written.",
        "i" = "Run {.code oa_publish()}, which renders before it deploys."
      ))
    }
  }
  # EVERY FILE AND WHERE IT GOES: the fragment and the numbers at the folder's root, the own exhibits below it
  own_ <- unlist(lapply(c("Tables", "Notes", "Figures"), \(.d) {
    dir_ <- fs::path(.dir_own, .d)
    if (!fs::dir_exists(dir_)) return(character(0))
    glob_ <- if (identical(.d, "Figures")) "*.pdf" else "*.tex"
    as.character(fs::dir_ls(dir_, glob = glob_, type = "file"))
  }))
  plan_ <- tibble::tibble(
    From = c(paths_, own_),
    Rel  = c(fs::path_file(paths_), as.character(fs::path_rel(own_, start = .dir_own)))
  )
  arch_    <- fs::path(.dir_deploy, "_archive", format(Sys.time(), "%Y%m%d-%H%M%S"))
  changed_ <- character(0)
  for (i_ in seq_len(nrow(plan_))) {
    target_ <- fs::path(.dir_deploy, plan_$Rel[[i_]])
    if (fs::file_exists(target_)) {
      if (oa_same_content(.a = target_, .b = plan_$From[[i_]])) next
      fs::dir_create(fs::path_dir(fs::path(arch_, plan_$Rel[[i_]])))
      fs::file_move(
        path     = target_,
        new_path = fs::path(arch_, plan_$Rel[[i_]])
      )
    }
    fs::dir_create(fs::path_dir(target_))
    fs::file_copy(
      path     = plan_$From[[i_]],
      new_path = target_
    )
    changed_ <- c(changed_, plan_$Rel[[i_]])
  }
  if (length(changed_) == 0L) {
    cli::cli_alert_info("Every file is identical to the deployed one: nothing to upload.")
    return(invisible(changed_))
  }
  if (fs::dir_exists(arch_)) cli::cli_alert_info("Replaced files archived to {.file {(arch_)}}.")
  cli::cli_alert_success("Deployed to {.file {(.dir_deploy)}}.")
  cli::cli_alert_info("Upload to Overleaf, into online_appendix/, keeping the subfolders: {.file {changed_}}.")
  invisible(changed_)
}

#' Render both formats and deploy: the one call that brings Overleaf up to date
#'
#' The Render button renders the html alone; this renders both passes through Quarto, in a separate R process as a
#' terminal render does, checks that the fragment and the numbers file were written by this call rather than left
#' over from an earlier one, and deploys them. It ends with the list of files to upload.
#'
#' @param .path_qmd Character. This document.
#' @param .path_tex Character. The fragment the LaTeX pass writes.
#' @param .path_numbers Character. The numbers file the render writes.
#' @param .dir_exhibits Character. 30's Output directory.
#' @param .dir_own Character. This document's own output directory.
#' @param .dir_deploy Character. The paper folder's OnlineAppendix directory.
#' @param .quarto Character or NULL. The Quarto binary; NULL tries QUARTO_PATH, the PATH, then RStudio's copy.
#' @return Invisibly, the names of the files that changed.
oa_publish <- function(.path_qmd, .path_tex, .path_numbers, .dir_exhibits, .dir_own, .dir_deploy, .quarto = NULL) {
  if (FALSE) {
    .path_qmd     <- here::here("1_code", "31-OnlineAppendix.qmd")
    .path_tex     <- here::here("_rendered", "1_code", "31-OnlineAppendix.tex")
    .path_numbers <- here::here("2_output", "31-OnlineAppendix", "Output", "OA-Numbers.tex")
    .dir_exhibits <- here::here("2_output", "30-FinalExhibits", "Output")
    .dir_own      <- here::here("2_output", "31-OnlineAppendix", "Output")
    .dir_deploy   <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "200-Paper_figures",
      "OnlineAppendix"
    )
    .quarto       <- NULL
  }
  bin_ <- oa_quarto_bin(.quarto = .quarto)
  cli::cli_alert_info("Rendering both formats with {.file {(bin_)}}.")
  t0_ <- Sys.time() - 1
  status_ <- system2(
    command = bin_,
    args    = c("render", shQuote(.path_qmd))
  )
  if (!identical(as.integer(status_), 0L)) {
    cli::cli_abort("The render failed with exit status {status_}; Quarto's output above says where.")
  }
  for (p_ in c(.path_tex, .path_numbers)) {
    if (!fs::file_exists(p_) || fs::file_info(p_)$modification_time < t0_) {
      cli::cli_abort("Quarto finished, but {.file {(p_)}} was not written by this render.")
    }
  }
  oa_deploy_tex(
    .path_tex     = .path_tex,
    .path_numbers = .path_numbers,
    .path_qmd     = .path_qmd,
    .dir_exhibits = .dir_exhibits,
    .dir_own      = .dir_own,
    .dir_deploy   = .dir_deploy
  )
}


# 8. Build: the exhibits this appendix writes itself ----------------------------------------------------------------
# FOR NOW, HERE. Some exhibits the appendix needs are not in 30. They are built here instead, into this document's own
# output directory in the layout 30 uses -- Tables/, Notes/, Figures/, Data/ -- and the fragment reads them through
# \oaown, as it reads 30's through \oaexhibits. Each is registered with Source "31", so moving one to 30 later changes
# one field of the registry and moves one function.
#
# A BUILD RUNS WHEN ITS INPUTS CHANGE. The data behind these exhibits -- the release, the master index -- is large and
# changes rarely, and the render runs twice. So a build compares its outputs with its inputs and with this library,
# and runs only when an output is missing or older; Rebuild forces every build.
#
# THE TABLE FRAME MIRRORS 30'S. fin_tex_write() lives in 30's library, which this document does not source. The
# frame is repeated here -- \arraystretch 1.5, \hline\hline on top, \midrule under the header, \hline at the bottom,
# widths by \dimexpr, no type size -- and moves back to one writer when these exhibits move to 30.

#' Whether a build has to run
#'
#' @param .outputs Character. The files the build writes.
#' @param .inputs Character. Files and directories it reads; a directory stands for its newest file.
#' @param .force Logical. TRUE runs the build regardless.
#' @return Logical scalar.
oa_build_needed <- function(.outputs, .inputs, .force) {
  if (FALSE) {
    .outputs <- fs::path_temp("x.tex")
    .inputs  <- here::here("1_code", "31-OnlineAppendix.R")
    .force   <- FALSE
  }
  if (isTRUE(.force) || !all(fs::file_exists(.outputs))) return(TRUE)
  newest_ <- function(.p) {
    if (!fs::file_exists(.p)) return(as.POSIXct(NA))
    if (fs::is_dir(.p)) {
      files_ <- fs::dir_ls(.p, recurse = TRUE, type = "file")
      if (length(files_) == 0L) return(as.POSIXct(NA))
      return(max(fs::file_info(files_)$modification_time))
    }
    fs::file_info(.p)$modification_time
  }
  t_in_  <- suppressWarnings(max(do.call(c, lapply(.inputs, newest_)), na.rm = TRUE))
  t_out_ <- min(fs::file_info(.outputs)$modification_time)
  isTRUE(t_in_ > t_out_)
}

#' A ragged-right p column of a given share of the text width
#'
#' Ragged right, because a justified narrow column spreads short phrases and breaks words; the width is set by
#' \dimexpr, as 30's frame sets its label column.
#'
#' @param .share Numeric. The share of \textwidth.
#' @return Character scalar: a column spec.
oa_col_text <- function(.share) {
  if (FALSE) .share <- 0.25
  paste0(">{\\raggedright\\arraybackslash}p{\\dimexpr", .share, "\\textwidth-2\\tabcolsep\\relax}")
}

#' A right-aligned number column of fixed width, as 30's frame sets number columns
#' @param .mm Numeric. The width in millimetres.
#' @return Character scalar: a column spec.
oa_col_num <- function(.mm) {
  if (FALSE) .mm <- 20
  paste0(">{\\raggedleft\\arraybackslash}p{", .mm, "mm}")
}

#' A tabular in the manuscript's frame
#'
#' Cells arrive escaped; a row whose Panel differs from the one above opens with a spanning italic panel line.
#'
#' @param .tab Tibble of character cells; a column Panel, if present, is set as panel lines and not as a column.
#' @param .header Character. The header cells, escaped.
#' @param .spec Character. One column spec per column of .tab without Panel.
#' @param .long Logical. TRUE sets a longtable, which breaks across pages and repeats its header.
#' @return Character: the lines of the tabular file.
oa_frame_table <- function(.tab, .header, .spec, .long = FALSE) {
  if (FALSE) {
    .tab    <- tibble::tibble(A = c("x", "y"), B = c("1", "2"))
    .header <- c("A", "B")
    .spec   <- c(oa_col_text(.share = 0.5), oa_col_num(.mm = 20))
    .long   <- FALSE
  }
  panel_ <- if ("Panel" %in% names(.tab)) .tab$Panel else rep(NA_character_, nrow(.tab))
  cells_ <- dplyr::select(.tab, -dplyr::any_of("Panel"))
  if (length(.spec) != ncol(cells_) || length(.header) != ncol(cells_)) {
    cli::cli_abort("Spec, header and cells disagree on the number of columns.")
  }
  rows_ <- purrr::map_chr(seq_len(nrow(cells_)), \(.i) paste0(paste(unlist(cells_[.i, ]), collapse = " & "), " \\\\"))
  open_ <- !is.na(panel_) & (seq_along(panel_) == 1L | panel_ != dplyr::lag(panel_, default = ""))
  body_ <- unlist(purrr::map(seq_along(rows_), \(.i) {
    if (open_[.i]) {
      c(paste0("\\multicolumn{", ncol(cells_), "}{l}{\\textit{", panel_[.i], "}} \\\\"), rows_[.i])
    } else {
      rows_[.i]
    }
  }))
  head_ <- c(
    "\\hline\\hline",
    paste0(paste(.header, collapse = " & "), " \\\\"),
    "\\midrule"
  )
  if (!isTRUE(.long)) {
    return(c(
      "\\begingroup\\renewcommand{\\arraystretch}{1.5}",
      paste0("\\begin{tabular}{", paste(.spec, collapse = " "), "}"),
      head_,
      body_,
      "\\hline",
      "\\end{tabular}",
      "\\endgroup"
    ))
  }
  # A TABLE LONGER THAN A PAGE: longtable, so the header repeats and the rows break where the page ends. The float
  # around it must not be a table environment, which cannot break; oa_float_tex() sets a long table on its own.
  c(
    "\\begingroup\\renewcommand{\\arraystretch}{1.5}",
    paste0("\\begin{longtable}{", paste(.spec, collapse = " "), "}"),
    head_,
    "\\endfirsthead",
    head_,
    "\\endhead",
    "\\hline",
    "\\endfoot",
    body_,
    "\\end{longtable}",
    "\\endgroup"
  )
}

#' Write an exhibit's files in 30's layout: the tabular or nothing, the note, the data
#'
#' @param .name Character. The exhibit's stem.
#' @param .lines Character or NULL. The tabular's lines; NULL for a figure, whose files plot_save() writes.
#' @param .note Character. The note, plain text.
#' @param .data Tibble. The data behind the exhibit, which the numbers read.
#' @param .dir_own Character. This document's output directory.
#' @return Invisibly, the paths written.
oa_write_exhibit <- function(.name, .lines, .note, .data, .dir_own) {
  if (FALSE) {
    .name    <- "RegulatoryTimeline"
    .lines   <- "x"
    .note    <- "y"
    .data    <- tibble::tibble(x = 1)
    .dir_own <- fs::path_temp()
  }
  fs::dir_create(fs::path(.dir_own, c("Tables", "Notes", "Figures", "Data")))
  out_ <- c(
    Note = fs::path(.dir_own, "Notes", paste0(.name, ".tex")),
    Data = fs::path(.dir_own, "Data", paste0(.name, ".parquet"))
  )
  writeLines(
    text = oa_tex_escape(.x = .note),
    con  = out_[["Note"]]
  )
  arrow::write_parquet(
    x    = .data,
    sink = out_[["Data"]]
  )
  if (!is.null(.lines)) {
    out_[["Table"]] <- fs::path(.dir_own, "Tables", paste0(.name, ".tex"))
    writeLines(
      text = .lines,
      con  = out_[["Table"]]
    )
  }
  invisible(out_)
}

#' The regulatory events that shape the database
#'
#' Events only, in date order; a constant rule such as the two-year lookback belongs to the text. Every row was
#' checked against the SEC release or the EDGAR documentation its note names, except two that rest on the database's
#' own data: the last 10-K405 in the master index, and the first order in EDGAR's listing.
#'
#' @return Tibble: Date, Rule, Meaning.
oa_data_timeline <- function() {
  tibble::tribble(
    ~Date,                 ~Rule,                                                     ~Meaning,
    "May 1996",            paste("EDGAR filing mandatory for all domestic registrants, after a phase-in from",
                                 "1993"),
                           "Filings are electronic; the master index lists them from 1993",
    "26 May 2000",         "EDGAR 7.0: each document of a filing stored as a file of its own",
                           paste("The database starts in 2001, the first full year; earlier filings exist only as",
                                 "complete submission text files"),
    "2003",                paste("EDGAR form type 10-K405 discontinued; the last in the master index is from",
                                 "2002"),
                           "Annual reports of 2001 and 2002 filed as 10-K405 lie outside the scope",
    "23 Aug 2004",         "Form 8-K reform: Item 1.01, four business days (Release 33-8400)",
                           paste("Every material contract is announced within four business days; the contract",
                                 "may follow with the next periodic report"),
    "Feb 2008 - Mar 2009", paste("Regulation S-B phased out (Release 33-8876): SB forms from 4 February 2008,",
                                 "Form 10-QSB from 31 October 2008, Form 10-KSB from 15 March 2009"),
                           paste("Small business issuers move to Forms S-1, 10-Q and 10-K, inside the scope; their",
                                 "earlier filings lie outside it"),
    "May 2008",            "Earliest confidential treatment order in EDGAR's listing",
                           "Redactions before the FAST Act can be traced to the order that granted them",
    "2 Apr 2019",          paste("FAST Act amendments to Item 601 (Release 33-10618): redaction without a",
                                 "confidential treatment request; immaterial schedules and attachments may be",
                                 "omitted; the two-year lookback limited to newly reporting registrants"),
                           paste("Redactions need no request and are reviewed selectively; contracts may arrive",
                                 "without their schedules; fewer old contracts are filed"),
    "15 Mar 2021",         paste("Redaction standard: information that is not material and that the registrant",
                                 "customarily and actually treats as private or confidential (Release 33-10884)"),
                           "The test for a permissible redaction changes within the post-FAST regime"
  )
}

#' Build the regulatory timeline
#'
#' @param .dir_own Character. This document's output directory.
#' @param .force Logical. TRUE rebuilds regardless of the inputs.
#' @param .path_lib Character. This library, an input of every build.
#' @return Invisibly, the build's status.
oa_build_timeline <- function(.dir_own, .force, .path_lib) {
  if (FALSE) {
    .dir_own  <- here::here("2_output", "31-OnlineAppendix", "Output")
    .force    <- FALSE
    .path_lib <- here::here("1_code", "31-OnlineAppendix.R")
  }
  name_ <- "RegulatoryTimeline"
  outs_ <- fs::path(.dir_own, c("Tables", "Notes", "Data"), paste0(name_, c(".tex", ".tex", ".parquet")))
  if (!oa_build_needed(.outputs = outs_, .inputs = .path_lib, .force = .force)) return(invisible("up to date"))
  tab_ <- oa_data_timeline()
  oa_write_exhibit(
    .name    = name_,
    .lines   = oa_frame_table(
      .tab    = tab_ |>
        dplyr::mutate(dplyr::across(dplyr::everything(), \(.x) oa_tex_escape(.x = .x))) |>
        dplyr::mutate(Date = gsub(" - ", " -- ", .data$Date, fixed = TRUE)),
      .header = c("Date", "Rule or event", "What it means for the database"),
      .spec   = c(oa_col_text(.share = 0.15), oa_col_text(.share = 0.44), oa_col_text(.share = 0.41))
    ),
    .note    = paste(
      "Sources: SEC Release Nos. 33-8400 (2004), 33-8876 (2007), 33-10618 (2019) and 33-10884 (2020); EDGAR's",
      "documentation of its archive; EDGAR's master index for the form types and its listing of confidential",
      "treatment orders."
    ),
    .data    = tab_,
    .dir_own = .dir_own
  )
  invisible("built")
}

#' The coverage groups: every EDGAR form type that carries material contracts, and whether the database covers it
#'
#' One row per form type. Panel A lists the forms the database draws on, Panel B the forms outside its scope. The
#' table states what the database and the master index hold: filings for every form, and contracts only for the
#' forms the database downloaded -- the contracts of forms outside the scope were never downloaded, so they are not
#' counted and not estimated. Form 20-F sits in Panel A: the database holds the contracts its filers number as
#' Exhibit 10, while the form's own instructions list material contracts as Exhibit 4, which the note says.
#'
#' @return Tibble: FormType, Panel, Group.
oa_coverage_forms <- function() {
  a_ <- "In the database"
  b_ <- "Outside it"
  tibble::tribble(
    ~FormType,   ~Panel, ~Group,
    "10-K",      a_,     "Annual reports",
    "10-K/A",    a_,     "Annual reports",
    "10-Q",      a_,     "Quarterly reports",
    "10-Q/A",    a_,     "Quarterly reports",
    "8-K",       a_,     "Current reports",
    "8-K/A",     a_,     "Current reports",
    "S-1",       a_,     "Registration statements",
    "S-1/A",     a_,     "Registration statements",
    "S-4",       a_,     "Registration statements",
    "S-4/A",     a_,     "Registration statements",
    "F-1",       a_,     "Registration statements",
    "F-1/A",     a_,     "Registration statements",
    "F-4",       a_,     "Registration statements",
    "F-4/A",     a_,     "Registration statements",
    "20-F",      a_,     "Foreign annual reports",
    "20-F/A",    a_,     "Foreign annual reports",
    "10QSB",     b_,     "Small business, periodic",
    "10QSB/A",   b_,     "Small business, periodic",
    "10KSB",     b_,     "Small business, periodic",
    "10KSB/A",   b_,     "Small business, periodic",
    "10KSB40",   b_,     "Small business, periodic",
    "10KSB40/A", b_,     "Small business, periodic",
    "SB-2",      b_,     "Small business, registration",
    "SB-2/A",    b_,     "Small business, registration",
    "SB-1",      b_,     "Small business, registration",
    "SB-1/A",    b_,     "Small business, registration",
    "10SB12G",   b_,     "Small business, registration",
    "10SB12G/A", b_,     "Small business, registration",
    "10SB12B",   b_,     "Small business, registration",
    "10SB12B/A", b_,     "Small business, registration",
    "10-K405",   b_,     "10-K variants",
    "10-K405/A", b_,     "10-K variants",
    "10KT405",   b_,     "10-K variants",
    "10-KT",     b_,     "10-K variants",
    "10-KT/A",   b_,     "10-K variants",
    "10-12G",    b_,     "Exchange Act registrations",
    "10-12G/A",  b_,     "Exchange Act registrations",
    "10-12B",    b_,     "Exchange Act registrations",
    "10-12B/A",  b_,     "Exchange Act registrations",
    "S-11",      b_,     "Real-estate registrations",
    "S-11/A",    b_,     "Real-estate registrations"
  )
}

#' The coverage table's data: filings per group, and the contracts the database holds
#'
#' @param .dir_master Character. The master index's parquet directory.
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @return Tibble: Panel, Group, Forms, Years, Filings, Contracts, Basis, Kind (group, total or design); one row per
#'   group, a total per panel, and one row for the forms out by design. Contracts is missing outside the database.
oa_data_coverage <- function(.dir_master, .path_contracts) {
  if (FALSE) {
    .dir_master <- rGetEDGAR::get_directories(
      here::here("2_output", "01A-EdgarIndex", "GetEDGAR")
    )$MasterIndex$DirParquet
    .path_contracts <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "Contracts.parquet"
    )
  }
  forms_ <- oa_coverage_forms()
  # FILINGS per form type, 2001-2024, from the master index
  fil_ <- arrow::open_dataset(sources = .dir_master) |>
    dplyr::filter(.data$FormType %in% forms_$FormType) |>
    dplyr::select("FormType", "DateFiled") |>
    dplyr::collect() |>
    dplyr::mutate(Year = as.integer(format(as.Date(.data$DateFiled), "%Y"))) |>
    dplyr::filter(dplyr::between(.data$Year, 2001L, 2024L)) |>
    dplyr::summarise(Filings = dplyr::n(), First = min(.data$Year), Last = max(.data$Year), .by = "FormType")
  # CONTRACTS the database holds: unique contracts of the descriptive sample, per form type
  held_ <- oa_read_sample(
    .path_contracts = .path_contracts,
    .cols           = "FormType"
  ) |>
    dplyr::count(.data$FormType, name = "Held")
  # THE FORMS OF A GROUP, in the order the list above gives them, amendments folded into one mention
  label_ <- function(.f) {
    orig_ <- .f[!grepl("/A$", .f)]
    paste0(paste(orig_, collapse = ", "), if (any(grepl("/A$", .f))) " (with /A)" else "")
  }
  out_ <- forms_ |>
    dplyr::left_join(fil_, by = "FormType") |>
    dplyr::left_join(held_, by = "FormType") |>
    dplyr::mutate(Filings = dplyr::coalesce(.data$Filings, 0L)) |>
    dplyr::summarise(
      Forms     = label_(.f = .data$FormType),
      Years     = paste0(min(.data$First, na.rm = TRUE), "-", max(.data$Last, na.rm = TRUE)),
      Filings   = sum(.data$Filings),
      Contracts = if (dplyr::first(.data$Panel) == "In the database") sum(.data$Held, na.rm = TRUE) else NA_real_,
      .by       = c("Panel", "Group")
    ) |>
    dplyr::mutate(
      Contracts = as.numeric(.data$Contracts),
      Basis     = dplyr::case_when(
        .data$Group == "Foreign annual reports" ~ "The release; Exhibit 10 only",
        .data$Panel == "In the database"        ~ "The release",
        .default                                = "Not downloaded"
      ),
      Kind = "group"
    )
  totals_ <- out_ |>
    dplyr::summarise(Filings = sum(.data$Filings), Contracts = sum(.data$Contracts), .by = "Panel") |>
    dplyr::mutate(Group = "Total", Forms = "", Years = "", Basis = "", Kind = "total")
  design_ <- tibble::tibble(
    Panel = "Out by design", Group = "Asset-backed issuers; foreign current reports",
    Forms = "SF-1, SF-3, 10-D; 6-K", Years = "", Filings = NA_integer_, Contracts = NA_real_,
    Basis = "Not operating firms; no exhibit numbering", Kind = "design"
  )
  dplyr::bind_rows(
    dplyr::filter(out_, .data$Panel == "In the database"),
    dplyr::filter(totals_, .data$Panel == "In the database"),
    dplyr::filter(out_, .data$Panel == "Outside it"),
    dplyr::filter(totals_, .data$Panel == "Outside it"),
    design_
  )
}

#' Build the coverage table
#'
#' @param .dir_master Character. The master index's parquet directory.
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @param .dir_own Character. This document's output directory.
#' @param .force Logical. TRUE rebuilds regardless of the inputs.
#' @param .path_lib Character. This library.
#' @return Invisibly, the build's status.
oa_build_coverage <- function(.dir_master, .path_contracts, .dir_own, .force, .path_lib) {
  if (FALSE) {
    .dir_master <- rGetEDGAR::get_directories(
      here::here("2_output", "01A-EdgarIndex", "GetEDGAR")
    )$MasterIndex$DirParquet
    .path_contracts <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "Contracts.parquet"
    )
    .dir_own  <- here::here("2_output", "31-OnlineAppendix", "Output")
    .force    <- FALSE
    .path_lib <- here::here("1_code", "31-OnlineAppendix.R")
  }
  name_ <- "FormCoverage"
  outs_ <- fs::path(.dir_own, c("Tables", "Notes", "Data"), paste0(name_, c(".tex", ".tex", ".parquet")))
  ins_  <- c(.dir_master, .path_contracts, .path_lib)
  if (!oa_build_needed(.outputs = outs_, .inputs = ins_, .force = .force)) return(invisible("up to date"))
  tab_ <- oa_data_coverage(
    .dir_master     = .dir_master,
    .path_contracts = .path_contracts
  )
  num_ <- function(.x) dplyr::if_else(is.na(.x), "", format(.x, big.mark = ",", trim = TRUE, scientific = FALSE))
  cells_ <- tab_ |>
    dplyr::transmute(
      Panel     = .data$Panel,
      Group     = oa_tex_escape(.x = .data$Group),
      Forms     = oa_tex_escape(.x = .data$Forms),
      Years     = gsub("-", "--", .data$Years, fixed = TRUE),
      Filings   = num_(.x = .data$Filings),
      Contracts = dplyr::if_else(.data$Panel == "Outside it", "--", num_(.x = .data$Contracts)),
      Basis     = oa_tex_escape(.x = .data$Basis)
    )
  tot_ <- tab_$Kind == "total"
  cells_[tot_, c("Group", "Filings", "Contracts")] <- lapply(cells_[tot_, c("Group", "Filings", "Contracts")],
                                                             \(.x) paste0("\\textbf{", .x, "}"))
  oa_write_exhibit(
    .name    = name_,
    .lines   = oa_frame_table(
      .tab    = cells_,
      .header = c("Group", "EDGAR form types", "Years", "Filings", "Contracts", "Basis"),
      .spec   = c(oa_col_text(.share = 0.19), oa_col_text(.share = 0.21), "l",
                  oa_col_num(.mm = 16), oa_col_num(.mm = 16), oa_col_text(.share = 0.15))
    ),
    .note    = paste(
      "Filings are counted in EDGAR's master index, 2001-2024; contracts are the unique contracts of the descriptive",
      "sample in the release. The forms outside the database were not downloaded, so their contracts are not",
      "counted. Form 20-F lists material contracts as Exhibit 4 (Instruction 4(a) of its Instructions as to",
      "Exhibits); the database holds the 20-F contracts that filers number as Exhibit 10."
    ),
    .data    = tab_,
    .dir_own = .dir_own
  )
  invisible("built")
}

#' The within-year figure's data: the share of each report type's contracts filed on each calendar day
#'
#' Unique contracts of the descriptive sample (30's Keep), 2001-2024. For every report type and year, the share of
#' that year's contracts filed on each day; the figure shows the mean over years, so every year counts equally.
#' February 29 is folded into February 28, so all years share one calendar; days without a filing count as zero.
#'
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @return Tibble: Group, Day (a date of 2025, standing for the calendar day), Share (percent).
oa_data_within_year <- function(.path_contracts) {
  if (FALSE) {
    .path_contracts <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "Contracts.parquet"
    )
  }
  groups_ <- tibble::tribble(
    ~FormType, ~Group,
    "8-K",     "Current reports (8-K)",
    "8-K/A",   "Current reports (8-K)",
    "10-K",    "Annual reports (10-K)",
    "10-K/A",  "Annual reports (10-K)",
    "10-Q",    "Quarterly reports (10-Q)",
    "10-Q/A",  "Quarterly reports (10-Q)"
  )
  con_ <- oa_read_sample(
    .path_contracts = .path_contracts,
    .cols           = "FormType"
  ) |>
    dplyr::inner_join(groups_, by = "FormType") |>
    dplyr::mutate(MD = sub("^02-29$", "02-28", format(.data$Date, "%m-%d")))
  days_ <- format(seq(as.Date("2025-01-01"), as.Date("2025-12-31"), by = "day"), "%m-%d")
  grid_ <- tidyr::expand_grid(Group = unique(groups_$Group), Year = 2001L:2024L, MD = days_)
  con_ |>
    dplyr::count(.data$Group, .data$Year, .data$MD, name = "N") |>
    dplyr::right_join(grid_, by = c("Group", "Year", "MD")) |>
    dplyr::mutate(N = dplyr::coalesce(.data$N, 0L)) |>
    dplyr::mutate(Share = 100 * .data$N / sum(.data$N), .by = c("Group", "Year")) |>
    dplyr::filter(!is.na(.data$Share)) |>
    dplyr::summarise(Share = mean(.data$Share), .by = c("Group", "MD")) |>
    dplyr::mutate(
      Day   = as.Date(paste0("2025-", .data$MD)),
      Group = factor(.data$Group, levels = unique(groups_$Group))
    ) |>
    dplyr::select("Group", "Day", "Share") |>
    dplyr::arrange(.data$Group, .data$Day)
}

#' The within-year figure: one panel per report type, one scale, the filing windows shaded
#'
#' @param .tab Tibble from oa_data_within_year().
#' @return A ggplot.
oa_plot_within_year <- function(.tab) {
  if (FALSE) .tab <- oa_data_within_year(.path_contracts = "Contracts.parquet")
  win_ <- tibble::tibble(
    Start = as.Date(c("2025-01-01", "2025-04-01", "2025-07-01", "2025-10-01")),
    End   = as.Date(c("2025-03-31", "2025-05-15", "2025-08-14", "2025-11-14")),
    Label = c("10-K window", "10-Q window", "10-Q window", "10-Q window"),
    Group = factor(levels(.tab$Group)[1L], levels = levels(.tab$Group))
  )
  top_ <- max(.tab$Share) * 1.08
  ggplot2::ggplot(.tab, ggplot2::aes(x = .data$Day, y = .data$Share)) +
    ggplot2::geom_rect(
      data        = dplyr::select(win_, -"Group"),
      mapping     = ggplot2::aes(xmin = .data$Start, xmax = .data$End, ymin = -Inf, ymax = Inf),
      inherit.aes = FALSE,
      fill        = "#EDEDED"
    ) +
    ggplot2::geom_text(
      data        = win_,
      mapping     = ggplot2::aes(x = .data$Start + (.data$End - .data$Start) / 2, y = top_, label = .data$Label),
      inherit.aes = FALSE,
      size        = 3.1,
      colour      = "#595959",
      family      = .plot_font,
      vjust       = 1
    ) +
    ggplot2::geom_line(colour = plot_pal_cat(.n = 1L), linewidth = 0.35) +
    ggplot2::facet_wrap(ggplot2::vars(.data$Group), ncol = 1L) +
    ggplot2::scale_x_date(
      breaks = seq(as.Date("2025-01-01"), as.Date("2025-12-01"), by = "month"),
      labels = \(.d) format(.d, "%b"),
      expand = c(0.005, 0.005)
    ) +
    ggplot2::scale_y_continuous(limits = c(0, top_), expand = c(0, 0)) +
    ggplot2::labs(x = NULL, y = "Share of the year's contracts filed on the day, in %") +
    plot_theme(.grid = "y", .legend = "none") +
    ggplot2::theme(strip.text = ggplot2::element_text(face = "bold", hjust = 0))
}

#' Build the within-year figure
#'
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @param .dir_own Character. This document's output directory.
#' @param .force Logical. TRUE rebuilds regardless of the inputs.
#' @param .path_lib Character. This library.
#' @return Invisibly, the build's status.
oa_build_within_year <- function(.path_contracts, .dir_own, .force, .path_lib) {
  if (FALSE) {
    .path_contracts <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "Contracts.parquet"
    )
    .dir_own        <- here::here("2_output", "31-OnlineAppendix", "Output")
    .force          <- FALSE
    .path_lib       <- here::here("1_code", "31-OnlineAppendix.R")
  }
  name_ <- "FilingsWithinYear"
  outs_ <- c(fs::path(.dir_own, "Figures", paste0(name_, c(".pdf", ".png"))),
             fs::path(.dir_own, c("Notes", "Data"), paste0(name_, c(".tex", ".parquet"))))
  ins_  <- c(.path_contracts, .path_lib)
  if (!oa_build_needed(.outputs = outs_, .inputs = ins_, .force = .force)) return(invisible("up to date"))
  tab_ <- oa_data_within_year(.path_contracts = .path_contracts)
  oa_write_exhibit(
    .name    = name_,
    .lines   = NULL,
    .note    = paste(
      "Unique contracts, 2001-2024, filed with current reports (8-K), annual reports (10-K) and quarterly reports",
      "(10-Q), each with its amendments. For each report type and year, the share of that year's contracts filed on",
      "each calendar day; the lines show the mean over years. Shaded are the filing windows of December year-end",
      "filers: 90 days after the fiscal year's end for the 10-K and 45 days after each quarter's end for the 10-Q.",
      "Within them, the deadlines of 60, 75 and 90 days (10-K) and 40 and 45 days (10-Q) differ by filer status."
    ),
    .data    = tab_,
    .dir_own = .dir_own
  )
  plot_save(
    .plot   = oa_plot_within_year(.tab = tab_),
    .name   = name_,
    .dir    = fs::path(.dir_own, "Figures"),
    .height = 5.2
  )
  invisible("built")
}

#' The unique contracts of the descriptive sample, as 30 selects them
#'
#' The one reader of the release every build here shares, so all of them count exactly the paper's sample: a
#' contract is kept where DescSample is 1 and it is the primary copy of its attachment -- 30's Keep. The flags are
#' compared after the collect, where an integer and a logical flag compare alike; in Arrow they need not.
#'
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @param .cols Character. The columns to keep besides the flags.
#' @return Tibble: .cols, plus Date and Year, for 2001-2024.
oa_read_sample <- function(.path_contracts, .cols) {
  if (FALSE) {
    .path_contracts <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "Contracts.parquet"
    )
    .cols <- c("FormType", "DateFiled")
  }
  arrow::open_dataset(sources = .path_contracts) |>
    dplyr::select(dplyr::all_of(unique(c(.cols, "DateFiled", "PrimaryFiler", "DescSample")))) |>
    dplyr::collect() |>
    dplyr::filter(
      dplyr::coalesce(as.integer(.data$PrimaryFiler), 0L) == 1L,
      dplyr::coalesce(as.integer(.data$DescSample), 0L) == 1L
    ) |>
    dplyr::mutate(
      Date = as.Date(.data$DateFiled),
      Year = as.integer(format(.data$Date, "%Y"))
    ) |>
    dplyr::filter(dplyr::between(.data$Year, 2001L, 2024L)) |>
    dplyr::select(-"PrimaryFiler", -"DescSample")
}

#' The seasoned-filer figure's data: four groups of filers, per year
#'
#' All filers; seasoned filers by 30's rule -- more than two years (2 x 365.25 days) past the CIK's first contract in
#' the sample, which leaves firms already filing in 2001 unseasoned until 2003; seasoned filers by the CIK's first
#' EDGAR filing of any form, from the master index, which starts in 1993 and does not date a firm by the filings
#' under study; and the contracts of blank-check registrants, the filings whose SIC code is 6770, from 01A's landing
#' pages. For each group and year: contracts, those filed with a registration statement (S-1, S-4, F-1, F-4 and
#' their amendments), and the equity contracts. The label is the release's Class, the crowned engine's detailed
#' label under a name that does not depend on the engine, which is also what 30 reads.
#'
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @param .path_landing Character. 01A's LandingPageAll.parquet, for the SIC code of each filing.
#' @param .dir_master Character. The master index's parquet directory.
#' @return Tibble: Year, Filers, N, NReg, ShareReg, NEquity, ShareEquity.
oa_data_seasoned <- function(.path_contracts, .path_landing, .dir_master) {
  if (FALSE) {
    .path_contracts <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "Contracts.parquet"
    )
    .path_landing <- here::here("2_output", "01A-EdgarIndex", "Output", "LandingPageAll.parquet")
    .dir_master   <- rGetEDGAR::get_directories(
      here::here("2_output", "01A-EdgarIndex", "GetEDGAR")
    )$MasterIndex$DirParquet
  }
  pad_ <- function(.x) sprintf("%010.0f", as.numeric(.x))
  con_ <- oa_read_sample(
    .path_contracts = .path_contracts,
    .cols           = c("CIK", "HashIndex", "FormType", "Class")
  ) |>
    dplyr::mutate(
      CIK      = pad_(.x = .data$CIK),
      IsReg    = sub("/A$", "", .data$FormType) %in% c("S-1", "S-4", "F-1", "F-4"),
      IsEquity = dplyr::coalesce(.data$Class == "Financial Instruments: Equity", FALSE)
    )
  if (!any(con_$IsEquity)) {
    cli::cli_abort("No contract's {.field Class} is {.val Financial Instruments: Equity}; the release's labels differ.")
  }
  # 30'S RULE: the first contract of the CIK in the sample
  con_ <- con_ |>
    dplyr::mutate(Entry = min(.data$Date), .by = "CIK") |>
    dplyr::mutate(Seasoned = as.numeric(.data$Date - .data$Entry) / 365.25 > 2)
  # THE CHECK: the first EDGAR filing of the CIK, any form, from the master index
  first_ <- arrow::open_dataset(sources = .dir_master) |>
    dplyr::select("CIK", "DateFiled") |>
    dplyr::mutate(Date = as.Date(.data$DateFiled)) |>
    dplyr::group_by(.data$CIK) |>
    dplyr::summarise(First = min(.data$Date, na.rm = TRUE)) |>
    dplyr::collect() |>
    dplyr::mutate(CIK = pad_(.x = .data$CIK)) |>
    dplyr::summarise(First = min(.data$First), .by = "CIK")
  con_ <- con_ |>
    dplyr::left_join(first_, by = "CIK") |>
    dplyr::mutate(SeasonedEdgar = dplyr::coalesce(as.numeric(.data$Date - .data$First) / 365.25 > 2, FALSE))
  # BLANK-CHECK REGISTRANTS: the filing's SIC code
  sic_ <- tibble::as_tibble(arrow::read_parquet(file = .path_landing, col_select = c("HashIndex", "SIC"))) |>
    dplyr::distinct(.data$HashIndex, .keep_all = TRUE)
  con_ <- con_ |>
    dplyr::left_join(sic_, by = "HashIndex") |>
    dplyr::mutate(BlankCheck = dplyr::coalesce(trimws(as.character(.data$SIC)) == "6770", FALSE))
  one_ <- function(.rows, .label) {
    .rows |>
      dplyr::summarise(
        N       = dplyr::n(),
        NReg    = sum(.data$IsReg),
        NEquity = sum(.data$IsEquity),
        .by     = "Year"
      ) |>
      tidyr::complete(Year = 2001L:2024L, fill = list(N = 0L, NReg = 0L, NEquity = 0L)) |>
      dplyr::mutate(Filers = .label)
  }
  dplyr::bind_rows(
    one_(.rows = con_,                                   .label = "All filers"),
    one_(.rows = dplyr::filter(con_, .data$Seasoned),      .label = "Seasoned filers"),
    one_(.rows = dplyr::filter(con_, .data$SeasonedEdgar), .label = "Seasoned, by first EDGAR filing"),
    one_(.rows = dplyr::filter(con_, .data$BlankCheck),    .label = "Blank-check registrants (SIC 6770)")
  ) |>
    dplyr::mutate(
      ShareReg    = dplyr::if_else(.data$N > 0L, .data$NReg / .data$N, NA_real_),
      ShareEquity = dplyr::if_else(.data$N > 0L, .data$NEquity / .data$N, NA_real_)
    ) |>
    dplyr::select("Year", "Filers", "N", "NReg", "ShareReg", "NEquity", "ShareEquity") |>
    dplyr::arrange(.data$Filers, .data$Year)
}

#' The seasoned-filer figure: contracts, registration share and equity share, all filers against seasoned filers
#'
#' @param .tab Tibble from oa_data_seasoned().
#' @return A ggplot.
oa_plot_seasoned <- function(.tab) {
  if (FALSE) .tab <- oa_data_seasoned(.path_contracts = "C", .path_landing = "L", .dir_master = "M")
  lv_ <- c("All filers", "Seasoned filers", "Seasoned, by first EDGAR filing", "Blank-check registrants (SIC 6770)")
  pa_ <- c("A. Contracts per year, in 1,000s", "B. Filed with a registration statement, in %",
           "C. Equity contracts, in %")
  two_ <- c("All filers", "Seasoned filers")
  dat_ <- dplyr::bind_rows(
    dplyr::transmute(.tab, .data$Year, .data$Filers, Value = .data$N / 1000, Panel = pa_[1L]),
    dplyr::transmute(dplyr::filter(.tab, .data$Filers %in% two_), .data$Year, .data$Filers,
                     Value = 100 * .data$ShareReg, Panel = pa_[2L]),
    dplyr::transmute(dplyr::filter(.tab, .data$Filers %in% two_), .data$Year, .data$Filers,
                     Value = 100 * .data$ShareEquity, Panel = pa_[3L])
  ) |>
    dplyr::filter(!is.na(.data$Value)) |>
    dplyr::mutate(
      Filers = factor(.data$Filers, levels = lv_),
      Panel  = factor(.data$Panel, levels = pa_)
    )
  cat_ <- plot_pal_cat(.n = 3L)
  ggplot2::ggplot(dat_, ggplot2::aes(x = .data$Year, y = .data$Value, colour = .data$Filers,
                                     linetype = .data$Filers)) +
    ggplot2::geom_line(linewidth = 0.5) +
    ggplot2::geom_point(size = 0.9) +
    ggplot2::facet_wrap(ggplot2::vars(.data$Panel), ncol = 1L, scales = "free_y") +
    ggplot2::scale_colour_manual(values = stats::setNames(cat_[c(1L, 2L, 2L, 3L)], lv_), drop = FALSE) +
    ggplot2::scale_linetype_manual(values = stats::setNames(c("solid", "solid", "dashed", "solid"), lv_), drop = FALSE) +
    ggplot2::scale_x_continuous(breaks = seq(2002L, 2024L, 2L), expand = c(0.01, 0.01)) +
    ggplot2::labs(x = NULL, y = NULL, colour = NULL, linetype = NULL) +
    plot_theme(.grid = "y", .legend = "bottom") +
    ggplot2::theme(strip.text = ggplot2::element_text(face = "bold", hjust = 0)) +
    ggplot2::guides(colour = ggplot2::guide_legend(nrow = 2L), linetype = ggplot2::guide_legend(nrow = 2L))
}

#' Build the seasoned-filer figure
#'
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @param .path_landing Character. 01A's LandingPageAll.parquet.
#' @param .dir_master Character. The master index's parquet directory.
#' @param .dir_own Character. This document's output directory.
#' @param .force Logical. TRUE rebuilds regardless of the inputs.
#' @param .path_lib Character. This library.
#' @return Invisibly, the build's status.
oa_build_seasoned <- function(.path_contracts, .path_landing, .dir_master, .dir_own, .force, .path_lib) {
  if (FALSE) {
    .path_contracts <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "Contracts.parquet"
    )
    .path_landing <- here::here("2_output", "01A-EdgarIndex", "Output", "LandingPageAll.parquet")
    .dir_master   <- rGetEDGAR::get_directories(
      here::here("2_output", "01A-EdgarIndex", "GetEDGAR")
    )$MasterIndex$DirParquet
    .dir_own      <- here::here("2_output", "31-OnlineAppendix", "Output")
    .force        <- FALSE
    .path_lib     <- here::here("1_code", "31-OnlineAppendix.R")
  }
  name_ <- "SeasonedFilers"
  outs_ <- c(fs::path(.dir_own, "Figures", paste0(name_, c(".pdf", ".png"))),
             fs::path(.dir_own, c("Notes", "Data"), paste0(name_, c(".tex", ".parquet"))))
  ins_  <- c(.path_contracts, .path_landing, .dir_master, .path_lib)
  if (!oa_build_needed(.outputs = outs_, .inputs = ins_, .force = .force)) return(invisible("up to date"))
  tab_ <- oa_data_seasoned(
    .path_contracts = .path_contracts,
    .path_landing   = .path_landing,
    .dir_master     = .dir_master
  )
  oa_write_exhibit(
    .name    = name_,
    .lines   = NULL,
    .note    = paste(
      "Unique contracts of the descriptive sample, 2001-2024. A seasoned filer is one more than two years past its",
      "first contract in the sample, so that firms already filing in 2001 count as seasoned from 2003; the dashed",
      "line dates a filer by its first EDGAR filing of any form instead, from EDGAR's master index, which starts in",
      "1993. Blank-check registrants are the filings under SIC code 6770. Registration statements are Forms S-1,",
      "S-4, F-1 and F-4 with their amendments; equity contracts are those classified as Financial Instruments:",
      "Equity."
    ),
    .data    = tab_,
    .dir_own = .dir_own
  )
  plot_save(
    .plot   = oa_plot_seasoned(.tab = tab_),
    .name   = name_,
    .dir    = fs::path(.dir_own, "Figures"),
    .height = 7.2
  )
  invisible("built")
}

#' The most frequent descriptions filers give their contracts, per category
#'
#' The filer's own description is the only human-written label an attachment carries, so it shows what a category
#' holds in the words of the people who file. Free text: filers write what they like, and many write nothing beyond
#' the exhibit number. Three rules make the descriptions comparable. A leading exhibit number is stripped, so that
#' "10.1 Credit Agreement" counts with "Credit Agreement". A description that carries no words beyond an exhibit
#' number or a form name is dropped as uninformative, and the share it accounts for is reported per category.
#' Descriptions are grouped case- and punctuation-insensitively, and the group is printed in its most frequent
#' original spelling.
#'
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @param .n Integer. Descriptions per category.
#' @return Tibble: Class, Kind, Rank, Description, N, Share (of the category's contracts that carry a description),
#'   plus nClass, nNamed and pNamed per category.
oa_data_titles <- function(.path_contracts, .n) {
  if (FALSE) {
    .path_contracts <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "Contracts.parquet"
    )
    .n <- 5L
  }
  # A LEADING EXHIBIT NUMBER, in the spellings filers use: EX-10.1, EXHIBIT 10.23, 10.1, (10.1), 10.1 -
  # It must carry a decimal point, or the word EX or EXHIBIT: a bare number is part of the title -- a year in
  # "2020 Equity Incentive Plan", a count in "3 Year Supply Agreement" -- and stripping it would corrupt the text.
  .re_number <- paste0(
    "(?i)^[\\s(\\[]*(",
    "ex(hibit)?[\\s.-]*\\d{1,3}([.(][a-z0-9]+\\)?)*",   # EX-10.1, EXHIBIT 10, EX 10.23(a), Ex. 10(a)
    "|\\d{1,3}([.(][a-z0-9]+\\)?)+",                    # 10.1, 10(a), 10.23a
    ")[\\s)\\]:.,-]*"
  )
  # NOTHING BUT A WORD FOR THE EXHIBIT ITSELF, or a form name: no description at all
  .re_unnamed <- paste0(
    "(?i)^(ex|exhibit|exhibits|document|attachment|annex|appendix|material contract[s]?|agreement|contract|",
    "8-k|10-k|10-q|20-f|s-1|s-4|f-1|f-4)$"
  )
  con_ <- oa_read_sample(
    .path_contracts = .path_contracts,
    .cols           = c("Class", "DocDesc")
  ) |>
    dplyr::filter(!is.na(.data$Class))
  if (nrow(con_) == 0L) cli::cli_abort("No contract carries a {.field Class}; the release's labels differ.")
  clean_ <- con_ |>
    dplyr::mutate(
      Desc = trimws(dplyr::coalesce(.data$DocDesc, "")),
      Desc = stringi::stri_replace_first_regex(.data$Desc, .re_number, ""),
      Desc = trimws(stringi::stri_replace_all_regex(.data$Desc, "\\s+", " ")),
      # UNINFORMATIVE: nothing left, or nothing but a form name or a word for the exhibit itself
      Named = nzchar(.data$Desc) & !stringi::stri_detect_regex(.data$Desc, .re_unnamed),
      Key = toupper(stringi::stri_replace_all_regex(.data$Desc, "[^[:alnum:] ]", " ")),
      Key = trimws(stringi::stri_replace_all_regex(.data$Key, "\\s+", " "))
    )
  per_class_ <- clean_ |>
    dplyr::summarise(nClass = dplyr::n(), pNamed = mean(.data$Named), .by = "Class")
  # A READABLE SPELLING. Filers most often write in capitals, and a table of capitals is hard to read and says
  # nothing the lower-case spelling does not. The most frequent spelling that is not all capitals is printed where
  # there is one; otherwise the capitals are set in title case, with the short words that belong inside a title
  # left lower-case.
  pretty_ <- function(.x) {
    small_ <- c("a", "an", "and", "as", "at", "by", "for", "from", "in", "of", "on", "or", "the", "to", "with")
    words_ <- strsplit(tolower(.x), " ", fixed = TRUE)[[1L]]
    up_    <- paste0(toupper(substring(words_, 1L, 1L)), substring(words_, 2L))
    out_   <- ifelse(words_ %in% small_ & seq_along(words_) > 1L, words_, up_)
    paste(out_, collapse = " ")
  }
  n_named_ <- clean_ |>
    dplyr::filter(.data$Named) |>
    dplyr::summarise(nNamed = dplyr::n(), .by = "Class")
  top_ <- clean_ |>
    dplyr::filter(.data$Named) |>
    dplyr::count(.data$Class, .data$Key, .data$Desc, name = "nSpelling") |>
    dplyr::arrange(dplyr::desc(.data$nSpelling)) |>
    dplyr::summarise(
      Description = {
        mixed_ <- .data$Desc[.data$Desc != toupper(.data$Desc)]
        if (length(mixed_) > 0L) mixed_[[1L]] else pretty_(.x = .data$Desc[[1L]])
      },
      N           = sum(.data$nSpelling),
      .by         = c("Class", "Key")
    ) |>
    dplyr::arrange(.data$Class, dplyr::desc(.data$N)) |>
    dplyr::slice_head(n = .n, by = "Class") |>
    dplyr::mutate(Rank = dplyr::row_number(), .by = "Class") |>
    dplyr::left_join(per_class_, by = "Class") |>
    dplyr::left_join(n_named_, by = "Class") |>
    dplyr::mutate(Share = .data$N / .data$nNamed, Kind = "title") |>
    dplyr::select("Class", "Kind", "Rank", "Description", "N", "Share", "nClass", "nNamed", "pNamed")
  dplyr::arrange(top_, .data$Class, .data$Rank)
}

#' Build the table of filer descriptions by category
#'
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @param .n Integer. Descriptions per category.
#' @param .dir_own Character. This document's output directory.
#' @param .force Logical. TRUE rebuilds regardless of the inputs.
#' @param .path_lib Character. This library.
#' @return Invisibly, the build's status.
oa_build_titles <- function(.path_contracts, .n, .dir_own, .force, .path_lib) {
  if (FALSE) {
    .path_contracts <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "Contracts.parquet"
    )
    .n        <- 5L
    .dir_own  <- here::here("2_output", "31-OnlineAppendix", "Output")
    .force    <- FALSE
    .path_lib <- here::here("1_code", "31-OnlineAppendix.R")
  }
  name_ <- "CategoryTitles"
  outs_ <- fs::path(.dir_own, c("Tables", "Notes", "Data"), paste0(name_, c(".tex", ".tex", ".parquet")))
  ins_  <- c(.path_contracts, .path_lib)
  if (!oa_build_needed(.outputs = outs_, .inputs = ins_, .force = .force)) return(invisible("up to date"))
  tab_ <- oa_data_titles(
    .path_contracts = .path_contracts,
    .n              = .n
  )
  cells_ <- tab_ |>
    dplyr::transmute(
      Panel       = paste0(oa_tex_escape(.x = .data$Class), " (",
                           formatC(100 * .data$pNamed, format = "f", digits = 0L), "\\% described)"),
      Description = oa_tex_escape(.x = .data$Description),
      N           = format(.data$N, big.mark = ",", trim = TRUE, scientific = FALSE),
      Share       = paste0(formatC(100 * .data$Share, format = "f", digits = 1L), "\\%")
    )
  oa_write_exhibit(
    .name    = name_,
    .lines   = oa_frame_table(
      .tab    = cells_,
      .header = c("Description", "Contracts", "Share"),
      .spec   = c(oa_col_text(.share = 0.55), oa_col_num(.mm = 22), oa_col_num(.mm = 18)),
      .long   = TRUE
    ),
    .note    = paste(
      "The", .n, "most frequent descriptions filers give their contracts, by category, over the unique contracts of",
      "the descriptive sample; the category is the classifier's label. A description is the filer's own free text,",
      "the only human-written label an attachment carries. A leading exhibit number is stripped before counting, and",
      "a description that says nothing beyond an exhibit or form name is treated as no description; the share of a",
      "category's contracts that carry one is given beside its name. Descriptions are grouped without regard to case",
      "and punctuation, and printed in the most frequent spelling that is not in capitals. Share is of the",
      "contracts in the category that carry a description."
    ),
    .data    = tab_,
    .dir_own = .dir_own
  )
  invisible("built")
}

#' The strongest terms of the published keyword table, per category
#'
#' 03C writes the published tables and a catalogue that names them, one row per task and context window. The
#' catalogue's row for the detailed task at the window the paper ships is the one read here, so this table and the
#' one the pipeline applies are the same file. Per category, the terms are those with the highest Power, the Wilson
#' lower bound on the term's precision in training, which is the order the table itself is sorted in.
#'
#' @param .dir_table Character. 03C's table directory, holding the published tables and catalogue.parquet.
#' @param .task Character. The task's label column, "ClassDetailed" for the contract type.
#' @param .n Integer. Terms per category.
#' @return Tibble: Class, Rank, Term, Power, Precision, HitsPos, Folds, plus nTerms per category and, as
#'   attributes of the run, the window and the promised precision the catalogue records.
oa_data_keyword_terms <- function(.dir_table, .task, .n) {
  if (FALSE) {
    .dir_table <- here::here("2_output", "03C-ClassifyTrainKeyword", "table")
    .task      <- "ClassDetailed"
    .n         <- 5L
  }
  cat_path_ <- fs::path(.dir_table, "catalogue.parquet")
  if (!fs::file_exists(cat_path_)) cli::cli_abort("No catalogue at {.file {(cat_path_)}}; 03C has not published.")
  cat_ <- tibble::as_tibble(arrow::read_parquet(file = cat_path_)) |>
    dplyr::filter(.data$Task == .task)
  if (nrow(cat_) == 0L) cli::cli_abort("The catalogue holds no row for task {.val {(.task)}}.")
  # THE WINDOW THAT SHIPS: the catalogue's row with the highest realised precision at full coverage of the
  # categories, which is the row 03C marks as its default; where several tie, the widest window.
  row_ <- cat_ |>
    dplyr::arrange(dplyr::desc(.data$Reached), dplyr::desc(.data$Realised), dplyr::desc(.data$NWords)) |>
    dplyr::slice_head(n = 1L)
  stem_ <- paste0(
    "keyword_table_",
    switch(.task, ClassDetailed = "detailed", ClassBroad = "broad", AmendType = "amendment", tolower(.task)),
    "_W",
    if (as.integer(row_$NWords) == 0L) "full" else as.character(as.integer(row_$NWords))
  )
  lex_path_ <- fs::path(.dir_table, paste0(stem_, ".parquet"))
  if (!fs::file_exists(lex_path_)) cli::cli_abort("The catalogue names {.file {stem_}}, which is not on disk.")
  lex_ <- tibble::as_tibble(arrow::read_parquet(file = lex_path_))
  need_ <- setdiff(c("Class", "Term", "Power", "Precision", "HitsPos", "Folds"), names(lex_))
  if (length(need_) > 0L) cli::cli_abort("The published table lacks {.field {need_}}.")
  out_ <- lex_ |>
    dplyr::mutate(nTerms = dplyr::n(), .by = "Class") |>
    dplyr::arrange(.data$Class, dplyr::desc(.data$Power), dplyr::desc(.data$HitsPos)) |>
    dplyr::slice_head(n = .n, by = "Class") |>
    dplyr::mutate(Rank = dplyr::row_number(), .by = "Class") |>
    dplyr::select("Class", "Rank", "Term", "Power", "Precision", "HitsPos", "Folds", "nTerms")
  attr(out_, "Window")   <- as.integer(row_$NWords)
  attr(out_, "Promised") <- as.numeric(row_$Promised)
  attr(out_, "Terms")    <- as.integer(row_$Terms)
  out_
}

#' Build the table of the keyword table's strongest terms
#'
#' @param .dir_table Character. 03C's table directory.
#' @param .n Integer. Terms per category.
#' @param .dir_own Character. This document's output directory.
#' @param .force Logical. TRUE rebuilds regardless of the inputs.
#' @param .path_lib Character. This library.
#' @return Invisibly, the build's status.
oa_build_keyword_terms <- function(.dir_table, .n, .dir_own, .force, .path_lib) {
  if (FALSE) {
    .dir_table <- here::here("2_output", "03C-ClassifyTrainKeyword", "table")
    .n         <- 5L
    .dir_own   <- here::here("2_output", "31-OnlineAppendix", "Output")
    .force     <- FALSE
    .path_lib  <- here::here("1_code", "31-OnlineAppendix.R")
  }
  if (!fs::file_exists(fs::path(.dir_table, "catalogue.parquet"))) {
    return(invisible("waiting for 03C's published tables"))
  }
  name_ <- "KeywordTerms"
  outs_ <- fs::path(.dir_own, c("Tables", "Notes", "Data"), paste0(name_, c(".tex", ".tex", ".parquet")))
  ins_  <- c(.dir_table, .path_lib)
  if (!oa_build_needed(.outputs = outs_, .inputs = ins_, .force = .force)) return(invisible("up to date"))
  tab_ <- oa_data_keyword_terms(
    .dir_table = .dir_table,
    .task      = "ClassDetailed",
    .n         = .n
  )
  cells_ <- tab_ |>
    dplyr::transmute(
      Panel     = oa_tex_escape(.x = .data$Class),
      Term      = oa_tex_escape(.x = .data$Term),
      Power     = formatC(.data$Power, format = "f", digits = 3L),
      Precision = formatC(.data$Precision, format = "f", digits = 3L),
      HitsPos   = format(.data$HitsPos, big.mark = ",", trim = TRUE, scientific = FALSE),
      Folds     = paste0(.data$Folds, "/5")
    )
  oa_write_exhibit(
    .name    = name_,
    .lines   = oa_frame_table(
      .tab    = cells_,
      .header = c("Term", "Power", "Precision", "Contracts", "Folds"),
      .spec   = c(oa_col_text(.share = 0.34), oa_col_num(.mm = 18), oa_col_num(.mm = 20),
                  oa_col_num(.mm = 20), oa_col_num(.mm = 14)),
      .long   = TRUE
    ),
    .note    = paste0(
      "The ", .n, " strongest terms of the published keyword table, by category, out of ", attr(tab_, "Terms"),
      " terms in all. Power is the Wilson lower bound on a term's precision among the training contracts it fires ",
      "on, which ranks a term found in 190 of 200 contracts above one found in 2 of 2; Precision is the raw share. ",
      "Contracts counts the training contracts of the category in which the term appears, and Folds the number of ",
      "the five fold-wise mines that selected it, of which at least three are required. The table reads the first ",
      if (identical(attr(tab_, "Window"), 0L)) "full text of a contract" else
        paste(attr(tab_, "Window"), "words of a contract"),
      " and promises a precision of ", formatC(100 * attr(tab_, "Promised"), format = "f", digits = 0L),
      " percent. The full table is part of the release."
    ),
    .data    = tab_,
    .dir_own = .dir_own
  )
  invisible("built")
}

#' The throughput of each extractor, as 04A's benchmark measured it
#'
#' THE ONE PLACE IN THIS APPENDIX WHERE A NUMBER IS TYPED RATHER THAN READ, and it is here because the measurement
#' is no longer in the data. 04A ran a batch sweep per family over a frozen 320-document draw and removed the
#' section once its ranking was settled; its bench table is empty by design, and its ledger records one timestamp
#' per run rather than one per batch, so neither can give a rate. The figures below are that benchmark's, and every
#' number in this appendix that comes from them says so.
#'
#' @return Tibble: Producer, DocsPerSec.
oa_ner_benchmark <- function() {
  tibble::tribble(
    ~Producer,   ~DocsPerSec,
    "matcon",    26.7,
    "lexnlp",    4.5,
    "spacy:trf", 2.1
  )
}

#' The extractor families compared, read from 04A's stores
#'
#' 04A writes one DuckDB database per family, and this reads them without changing anything: the entity tables hold
#' every span with its offsets, and the ledger records when each document was written, per model, which is what the
#' throughput below is recovered from.
#'
#' Position is the midpoint of a span over the length of its document, as 04A defines it. The concentration reported
#' here is the mean share of a producer's organisation names in the first and last tenth of a contract over their
#' mean share through the middle, excluding the second and ninth deciles as shoulder, which is 04A's rule.
#'
#' @param .dir_store Character. 04A's Output directory, holding one <family>.duckdb per family.
#' @param .path_text Character. 04A's sample_text.parquet, for document lengths.
#' @param .families Character. The families to read.
#' @return Tibble: Producer, Family, Label, Documents, DocsWithSpan, Spans, Entities, Seconds, DocsPerSec,
#'   RateSource (ledger or benchmark), ContrastOrg.
oa_data_ner_engines <- function(.dir_store, .path_text, .families) {
  if (FALSE) {
    .dir_store <- here::here("2_output", "04A-EntityExtract", "Output")
    .path_text <- here::here("2_output", "04A-EntityExtract", "Output", "sample_text.parquet")
    .families  <- c("matcon", "lexnlp", "spacy")
  }
  paths_ <- fs::path(.dir_store, paste0(.families, ".duckdb"))
  names(paths_) <- .families
  have_ <- paths_[fs::file_exists(paths_)]
  if (length(have_) == 0L) cli::cli_abort("No family database under {.file {(.dir_store)}}.")
  meta_ <- c("runs", "manifest", "bench", "corpus", "failures")
  # THE PRODUCER, as 04A names it: the family, and the model too where the family is spaCy.
  short_ <- function(.model) paste0("spacy:", stringi::stri_replace_first_fixed(.model, "en_core_web_", ""))
  one_ <- function(.family, .path) {
    con_ <- DBI::dbConnect(duckdb::duckdb(dbdir = as.character(.path), read_only = TRUE))
    on.exit(DBI::dbDisconnect(con_, shutdown = TRUE), add = TRUE)
    tabs_ <- setdiff(DBI::dbListTables(con_), meta_)
    if (length(tabs_) == 0L) return(NULL)
    spans_ <- purrr::map(tabs_, \(.t) {
      cols_ <- DBI::dbListFields(con_, .t)
      sel_  <- if ("Model" %in% cols_) "Model" else "'-' AS Model"
      # THE TABLE NAME IS THE ENTITY, stored lowercase by 04A and shown upper by it; upper here too, so a filter
      # written as ORG matches what 04A's own reports call ORG.
      DBI::dbGetQuery(con_, paste0("SELECT DocID, Start, Stop, ", sel_, ", '", toupper(.t), "' AS Entity FROM ", .t))
    }) |>
      purrr::list_rbind() |>
      tibble::as_tibble()
    if (nrow(spans_) == 0L) return(NULL)
    # THE LEDGER'S TIMESTAMPS, because the bench table is no longer written: 04A removed the benchmark section once
    # its ranking was settled, and left the table in place empty. The ledger records when each document was written,
    # per model, so the time a run took can be recovered from it.
    runs_ <- if ("runs" %in% DBI::dbListTables(con_)) {
      tibble::as_tibble(DBI::dbGetQuery(con_, "SELECT DISTINCT DocID, Model, RunAt FROM runs"))
    } else {
      tibble::tibble()
    }
    list(
      Spans = dplyr::mutate(spans_, Family = .family),
      Runs  = if (nrow(runs_) == 0L) runs_ else dplyr::mutate(runs_, Family = .family)
    )
  }
  read_ <- purrr::compact(purrr::imap(have_, \(.p, .f) one_(.family = .f, .path = .p)))
  spans_ <- purrr::list_rbind(purrr::map(read_, "Spans")) |>
    dplyr::mutate(Producer = dplyr::if_else(.data$Family == "spacy", short_(.model = .data$Model), .data$Family))
  # DOCUMENT LENGTHS, so a span's position is comparable across contracts
  len_ <- tibble::as_tibble(arrow::read_parquet(file = .path_text, col_select = c("DocID", "TextRaw"))) |>
    dplyr::transmute(DocID = .data$DocID, DocLen = nchar(.data$TextRaw)) |>
    dplyr::filter(.data$DocLen > 0L)
  # SHARES PER DECILE, THEN THE MEAN OF EACH ZONE, which is 04A's rule and needs no constant of its own: the end
  # zone holds two deciles and the middle six, so comparing zone totals would answer a different question.
  contrast_ <- spans_ |>
    dplyr::filter(.data$Entity == "ORG", !is.na(.data$Start)) |>
    dplyr::inner_join(len_, by = "DocID") |>
    dplyr::mutate(Bin = pmin(9L, as.integer(floor((((.data$Start + .data$Stop) / 2) / .data$DocLen) * 10)))) |>
    dplyr::count(.data$Producer, .data$Bin, name = "Spans") |>
    tidyr::complete(Producer = unique(spans_$Producer), Bin = 0L:9L, fill = list(Spans = 0L)) |>
    dplyr::mutate(Share = .data$Spans / sum(.data$Spans), .by = "Producer") |>
    dplyr::mutate(Zone = dplyr::case_when(
      .data$Bin %in% c(0L, 9L)  ~ "End",
      .data$Bin %in% 2L:7L      ~ "Mid",
      .default                  = "Shoulder"
    )) |>
    dplyr::filter(.data$Zone != "Shoulder") |>
    dplyr::summarise(Share = mean(.data$Share), .by = c("Producer", "Zone")) |>
    tidyr::pivot_wider(id_cols = "Producer", names_from = "Zone", values_from = "Share") |>
    dplyr::filter(.data$Mid > 0) |>
    dplyr::mutate(ContrastOrg = .data$End / .data$Mid) |>
    dplyr::select("Producer", "ContrastOrg")
  # HOW LONG A RUN TOOK, from the ledger. The write timestamps of one model are sorted and the intervals between
  # them summed, counting an interval only where it is shorter than the cutoff: a run resumed the next day must not
  # be counted as running overnight, and the ledger is built precisely so that a run can be resumed. What this
  # measures is therefore the time the extraction was working, on the machine it ran on, and not a controlled
  # benchmark on identical hardware.
  runs_ <- purrr::list_rbind(purrr::map(read_, "Runs"))
  rate_ <- tibble::tibble(Producer = character(0), Documents = integer(0), Seconds = numeric(0),
                          DocsPerSec = numeric(0))
  if (nrow(runs_) > 0L) {
    cut_  <- 600  # seconds; an interval longer than this is a pause, not work
    rate_ <- runs_ |>
      dplyr::mutate(
        Producer = dplyr::if_else(.data$Family == "spacy", short_(.model = .data$Model), .data$Family),
        RunAt    = as.POSIXct(.data$RunAt)
      ) |>
      dplyr::summarise(
        Documents = dplyr::n_distinct(.data$DocID),
        Stamps    = dplyr::n_distinct(.data$RunAt),
        Seconds   = {
          t_ <- sort(unique(.data$RunAt))
          if (length(t_) < 2L) NA_real_ else sum(diff(as.numeric(t_))[diff(as.numeric(t_)) <= cut_])
        },
        .by = "Producer"
      ) |>
      dplyr::mutate(
        # TOO FEW WRITES TO TIME: a model whose rows all carry one timestamp says nothing about how long it took.
        DocsPerSec = dplyr::if_else(.data$Stamps >= 20L & .data$Seconds > 0, .data$Documents / .data$Seconds,
                                    NA_real_)
      ) |>
      dplyr::select("Producer", "Documents", "Seconds", "DocsPerSec")
  }
  spans_ |>
    dplyr::summarise(
      Family      = dplyr::first(.data$Family),
      DocsWithSpan = dplyr::n_distinct(.data$DocID),
      Spans       = dplyr::n(),
      Entities    = dplyr::n_distinct(.data$Entity),
      .by         = "Producer"
    ) |>
    dplyr::left_join(rate_, by = "Producer") |>
    dplyr::left_join(dplyr::rename(oa_ner_benchmark(), Declared = "DocsPerSec"), by = "Producer") |>
    dplyr::mutate(
      Documents  = dplyr::coalesce(.data$Documents, .data$DocsWithSpan),
      # THE LEDGER FIRST, the benchmark where it has nothing: a rate this render measured beats one carried in text.
      RateSource = dplyr::case_when(
        !is.na(.data$DocsPerSec) ~ "ledger",
        !is.na(.data$Declared)   ~ "benchmark",
        .default                 = NA_character_
      ),
      DocsPerSec = dplyr::coalesce(.data$DocsPerSec, .data$Declared)
    ) |>
    dplyr::left_join(contrast_, by = "Producer") |>
    dplyr::mutate(
      Label = dplyr::case_match(
        .data$Producer,
        "matcon"    ~ "Pattern and gazetteer extractors",
        "lexnlp"    ~ "LexNLP",
        "spacy:sm"  ~ "spaCy, small",
        "spacy:md"  ~ "spaCy, medium",
        "spacy:lg"  ~ "spaCy, large",
        "spacy:trf" ~ "spaCy, transformer",
        .default    = .data$Producer
      )
    ) |>
    dplyr::arrange(dplyr::desc(.data$DocsPerSec), dplyr::desc(.data$Spans))
}

#' Build the table comparing the extractor families
#'
#' @param .dir_store Character. 04A's Output directory.
#' @param .path_text Character. 04A's sample_text.parquet.
#' @param .dir_own Character. This document's output directory.
#' @param .force Logical. TRUE rebuilds regardless of the inputs.
#' @param .path_lib Character. This library.
#' @return Invisibly, the build's status.
oa_build_ner_engines <- function(.dir_store, .path_text, .dir_own, .force, .path_lib) {
  if (FALSE) {
    .dir_store <- here::here("2_output", "04A-EntityExtract", "Output")
    .path_text <- here::here("2_output", "04A-EntityExtract", "Output", "sample_text.parquet")
    .dir_own   <- here::here("2_output", "31-OnlineAppendix", "Output")
    .force     <- FALSE
    .path_lib  <- here::here("1_code", "31-OnlineAppendix.R")
  }
  fams_ <- c("matcon", "lexnlp", "spacy")
  dbs_  <- fs::path(.dir_store, paste0(fams_, ".duckdb"))
  if (!any(fs::file_exists(dbs_)) || !fs::file_exists(.path_text)) {
    return(invisible("waiting for 04A's entity stores"))
  }
  name_ <- "NerEngines"
  outs_ <- fs::path(.dir_own, c("Tables", "Notes", "Data"), paste0(name_, c(".tex", ".tex", ".parquet")))
  ins_  <- c(dbs_[fs::file_exists(dbs_)], .path_text, .path_lib)
  if (!oa_build_needed(.outputs = outs_, .inputs = ins_, .force = .force)) return(invisible("up to date"))
  tab_ <- oa_data_ner_engines(
    .dir_store = .dir_store,
    .path_text = .path_text,
    .families  = fams_
  )
  # A DASH, NOT A BLANK: the pattern extractors propose no organisation names, so the comparison has no value for
  # them, which is different from a value that failed to compute.
  fmt_ <- function(.x, .d) dplyr::if_else(is.na(.x), "--", formatC(.x, format = "f", digits = .d, big.mark = ","))
  # A COLUMN OF DASHES SAYS NOTHING. Where the ledger records one timestamp per run rather than one per batch, no
  # throughput can be recovered from it, and the column is left out rather than printed empty; the note says so.
  timed_ <- any(!is.na(tab_$DocsPerSec))
  cells_ <- tab_ |>
    dplyr::transmute(
      Extractor   = oa_tex_escape(.x = .data$Label),
      Documents   = format(.data$Documents, big.mark = ",", trim = TRUE, scientific = FALSE),
      Spans       = format(.data$Spans, big.mark = ",", trim = TRUE, scientific = FALSE),
      DocsPerSec  = fmt_(.x = .data$DocsPerSec, .d = 1L),
      ContrastOrg = fmt_(.x = .data$ContrastOrg, .d = 2L)
    )
  if (!timed_) cells_ <- dplyr::select(cells_, -"DocsPerSec")
  oa_write_exhibit(
    .name    = name_,
    .lines   = oa_frame_table(
      .tab    = cells_,
      .header = c("Extractor", "Documents", "Spans", if (timed_) "Per second", "Concentration"),
      .spec   = c(oa_col_text(.share = 0.28), oa_col_num(.mm = 20), oa_col_num(.mm = 20),
                  if (timed_) oa_col_num(.mm = 18), oa_col_num(.mm = 22))
    ),
    .note    = paste0(
      "The families of extractors on the contracts of the labelled sample. Spans counts everything the extractor ",
      "proposed, over every entity type it produces. ",
      if (timed_ && all(stats::na.omit(tab_$RateSource) == "benchmark")) paste0(
        "Per second is the documents an extractor read in a second, measured by a benchmark over a frozen draw of ",
        "320 contracts at the batch size and worker count the pipeline uses. "
      ) else if (timed_) paste0(
        "Per second is the documents an extractor read in a second, recovered from the extraction ledger where it ",
        "records the progress of a run and from a benchmark over a frozen draw of 320 contracts otherwise. "
      ),
      "Concentration at the ends is the mean share of a ",
      "producer's organisation names in the first and last tenth of a contract over their mean share through the ",
      "middle, which is where the parties to a contract are named; a producer that spreads them evenly through the ",
      "text scores 1.0 by construction. The pattern extractors propose no organisation names, so the comparison ",
      "does not apply to them."
    ),
    .data    = tab_,
    .dir_own = .dir_own
  )
  invisible("built")
}

#' Where each contract's end date comes from, and what the two durations look like
#'
#' The cascade answers from the first of four sources present, so the rungs partition the sample: a stated term, an
#' open-ended clause, which establishes that there is no end date, a future date beside a termination cue, and the
#' farthest future date. The naive measure uses the last of these alone. Both durations run from the same start, so
#' they differ only in the end they take, and the quartiles below are over the contracts each measure is defined on.
#'
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @return Tibble: Rung, Label, N, Share, plus the quartiles of the rule-based and the naive duration.
oa_data_duration <- function(.path_contracts) {
  if (FALSE) {
    .path_contracts <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "Contracts.parquet"
    )
  }
  con_ <- oa_read_sample(
    .path_contracts = .path_contracts,
    .cols           = c("DurationSource", "DurationDropped", "DurationYears", "NaiveYears")
  )
  rungs_ <- tibble::tribble(
    ~Rung,     ~Label,
    "term",    "A stated term",
    "open",    "An open-ended clause",
    "cue",     "A future date beside a termination cue",
    "maxdate", "The farthest future date",
    "none",    "No end established"
  )
  q_ <- function(.x, .p) {
    x_ <- .x[!is.na(.x)]
    if (length(x_) == 0L) return(NA_real_)
    unname(stats::quantile(x_, probs = .p, type = 7L))
  }
  n_all_ <- nrow(con_)
  by_rung_ <- con_ |>
    dplyr::mutate(Rung = dplyr::coalesce(.data$DurationSource, "none")) |>
    dplyr::summarise(
      N       = dplyr::n(),
      Defined = sum(!is.na(.data$DurationYears)),
      Q1      = q_(.x = .data$DurationYears, .p = 0.25),
      Med     = q_(.x = .data$DurationYears, .p = 0.50),
      Q3      = q_(.x = .data$DurationYears, .p = 0.75),
      .by     = "Rung"
    )
  out_ <- rungs_ |>
    dplyr::left_join(by_rung_, by = "Rung") |>
    dplyr::mutate(dplyr::across(c("N", "Defined"), \(.x) dplyr::coalesce(.x, 0L))) |>
    dplyr::mutate(Kind = "rung")
  # THE TWO MEASURES, each over the contracts it is defined on, so the comparison is of what each one yields
  totals_ <- tibble::tibble(
    Rung    = c("all-rule", "all-naive"),
    Label   = c("Rule-based duration", "Naive duration"),
    N       = c(n_all_, n_all_),
    Defined = c(sum(!is.na(con_$DurationYears)), sum(!is.na(con_$NaiveYears))),
    Q1      = c(q_(.x = con_$DurationYears, .p = 0.25), q_(.x = con_$NaiveYears, .p = 0.25)),
    Med     = c(q_(.x = con_$DurationYears, .p = 0.50), q_(.x = con_$NaiveYears, .p = 0.50)),
    Q3      = c(q_(.x = con_$DurationYears, .p = 0.75), q_(.x = con_$NaiveYears, .p = 0.75)),
    Kind    = "measure"
  )
  # A COMPLETE ACCOUNT OF WHAT IS MISSING. The panel is built over the contracts that have no duration, so its rows
  # sum to exactly that number; a contract whose reason the release does not record is a row of its own rather than
  # a silent remainder.
  dropped_ <- con_ |>
    dplyr::filter(is.na(.data$DurationYears)) |>
    dplyr::mutate(Reason = dplyr::coalesce(.data$DurationDropped, "unrecorded")) |>
    dplyr::mutate(Reason = dplyr::if_else(.data$Reason == "kept", "unrecorded", .data$Reason)) |>
    dplyr::count(.data$Reason, name = "N") |>
    dplyr::transmute(
      Rung    = paste0("dropped-", .data$Reason),
      Label   = dplyr::case_match(
        .data$Reason,
        "capped"     ~ "Longer than thirty years, dropped",
        "negative"   ~ "The end precedes the start, dropped",
        "no end"     ~ "No end date established",
        "unrecorded" ~ "No reason recorded",
        .default     = .data$Reason
      ),
      N       = .data$N,
      Defined = 0L,
      Q1      = NA_real_,
      Med     = NA_real_,
      Q3      = NA_real_,
      Kind    = "dropped"
    ) |>
    dplyr::arrange(dplyr::desc(.data$N))
  dplyr::bind_rows(out_, dropped_, totals_) |>
    dplyr::mutate(Share = .data$N / n_all_, Contracts = n_all_) |>
    dplyr::select("Rung", "Label", "Kind", "N", "Share", "Defined", "Q1", "Med", "Q3", "Contracts")
}

#' Build the table of duration sources
#'
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @param .dir_own Character. This document's output directory.
#' @param .force Logical. TRUE rebuilds regardless of the inputs.
#' @param .path_lib Character. This library.
#' @return Invisibly, the build's status.
oa_build_duration <- function(.path_contracts, .dir_own, .force, .path_lib) {
  if (FALSE) {
    .path_contracts <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "Contracts.parquet"
    )
    .dir_own  <- here::here("2_output", "31-OnlineAppendix", "Output")
    .force    <- FALSE
    .path_lib <- here::here("1_code", "31-OnlineAppendix.R")
  }
  name_ <- "DurationRungs"
  outs_ <- fs::path(.dir_own, c("Tables", "Notes", "Data"), paste0(name_, c(".tex", ".tex", ".parquet")))
  if (!oa_build_needed(.outputs = outs_, .inputs = c(.path_contracts, .path_lib), .force = .force)) {
    return(invisible("up to date"))
  }
  tab_ <- oa_data_duration(.path_contracts = .path_contracts)
  num_ <- function(.x) format(.x, big.mark = ",", trim = TRUE, scientific = FALSE)
  yrs_ <- function(.x) dplyr::if_else(is.na(.x), "--", formatC(.x, format = "f", digits = 1L))
  panel_ <- c(rung = "Which rung answered", dropped = "Why a duration is missing",
              measure = "The two measures, over the contracts each is defined on")
  cells_ <- tab_ |>
    dplyr::transmute(
      Panel   = unname(panel_[.data$Kind]),
      Label   = oa_tex_escape(.x = .data$Label),
      N       = num_(.x = .data$N),
      Share   = paste0(formatC(100 * .data$Share, format = "f", digits = 1L), "\\%"),
      Defined = dplyr::if_else(.data$Kind == "dropped", "--", num_(.x = .data$Defined)),
      Q1      = yrs_(.x = .data$Q1),
      Med     = yrs_(.x = .data$Med),
      Q3      = yrs_(.x = .data$Q3)
    )
  oa_write_exhibit(
    .name    = name_,
    .lines   = oa_frame_table(
      .tab    = cells_,
      .header = c("Source", "Contracts", "Share", "Defined", "25th", "Median", "75th"),
      .spec   = c(oa_col_text(.share = 0.25), oa_col_num(.mm = 16), oa_col_num(.mm = 12),
                  oa_col_num(.mm = 15), oa_col_num(.mm = 11), oa_col_num(.mm = 13), oa_col_num(.mm = 11))
    ),
    .note    = paste(
      "The unique contracts of the descriptive sample. The cascade takes the first source present, so the rungs",
      "partition the sample: a stated term; an open-ended clause, which establishes that the contract has no end",
      "date and therefore no duration; a future date within a termination cue's reach; and the farthest future date.",
      "Defined counts the contracts of the row for which a duration could be computed, and the quartiles are in",
      "years, over those contracts. Shares are of all contracts throughout, so the second panel accounts for every",
      "contract that lacks a duration and the first for every contract.",
      "The naive duration takes the farthest future date for every contract, and both",
      "measures run from the same start date, so they differ only in the end they take."
    ),
    .data    = tab_,
    .dir_own = .dir_own
  )
  invisible("built")
}

#' The counts the text of Appendix A cites, which no table prints
#'
#' Three passages name numbers that belong to no exhibit: what each quality rule flags, what the confidential
#' treatment orders cover, and how many Item 1.01 announcements the sample keeps. They are computed here, written as
#' a tibble like any other exhibit's data, and read by the numbers spec; nothing is printed.
#'
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @param .path_orders Character. The release's CtoOrders.parquet, one row per reference an order makes.
#' @return Tibble: Key, Value.
oa_data_counts <- function(.path_contracts, .path_orders) {
  if (FALSE) {
    .path_contracts <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "Contracts.parquet"
    )
    .path_orders <- fs::path(fs::path_dir(.path_contracts), "CtoOrders.parquet")
  }
  all_ <- arrow::open_dataset(sources = .path_contracts) |>
    dplyr::select(dplyr::any_of(c("DocID", "PrimaryFiler", "DescSample", "Removed", "RemClass", "HasCto",
                                  "SumWords", "nItems", "nWords"))) |>
    dplyr::collect() |>
    dplyr::mutate(
      Primary = dplyr::coalesce(as.integer(.data$PrimaryFiler), 0L) == 1L,
      Flagged = dplyr::coalesce(as.integer(.data$Removed), 0L) == 1L
    )
  uni_ <- dplyr::filter(all_, .data$Primary)
  rule_ <- uni_ |>
    dplyr::filter(.data$Flagged, !is.na(.data$RemClass)) |>
    dplyr::count(.data$RemClass, name = "N")
  pick_ <- function(.prefix) {
    row_ <- rule_[startsWith(rule_$RemClass, .prefix), , drop = FALSE]
    if (nrow(row_) == 0L) return(0)
    sum(row_$N)
  }
  # THE ITEM 1.01 NARRATIVES, which 01D recovers from the 8-K that announced the contract: a contract has one where
  # the release records the words of its summary.
  sum_ <- if ("SumWords" %in% names(uni_)) sum(!is.na(uni_$SumWords)) else NA_real_
  # THE ORDERS, one row per reference an order makes to an exhibit. A reference is resolved where it found the
  # attachment it names; the rest fail at a named point in the chain, which LinkStatus records.
  ord_ <- tibble::tibble(Key = character(0), Value = numeric(0))
  if (fs::file_exists(.path_orders)) {
    ref_ <- arrow::open_dataset(sources = .path_orders) |>
      dplyr::select(dplyr::any_of(c("OrderDocID", "DocID", "LinkStatus", "IsExtension", "Status"))) |>
      dplyr::collect()
    ord_ <- tibble::tibble(
      Key = c("Orders", "OrderRefs", "OrderRefsLinked", "OrderRefsUnresolved"),
      Value = c(
        dplyr::n_distinct(ref_$OrderDocID),
        nrow(ref_),
        sum(ref_$LinkStatus == "A-linked"),
        sum(ref_$LinkStatus != "A-linked")
      )
    )
  }
  dplyr::bind_rows(
    tibble::tibble(
      Key = c("DocsAll", "DocsUnique", "DocsFlagged", "RuleShort", "RuleStopwords", "RuleNumeric", "DocsWithCto",
              "DocsWithSummary"),
      Value = c(
        nrow(all_),
        nrow(uni_),
        sum(uni_$Flagged),
        pick_(.prefix = "1-"),
        pick_(.prefix = "2-"),
        pick_(.prefix = "3-"),
        if ("HasCto" %in% names(uni_)) sum(dplyr::coalesce(as.integer(uni_$HasCto), 0L) == 1L) else NA_real_,
        sum_
      )
    ),
    ord_
  )
}

#' Build the counts the text of Appendix A cites
#'
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @param .path_orders Character. The release's CtoOrders.parquet.
#' @param .dir_own Character. This document's output directory.
#' @param .force Logical. TRUE rebuilds regardless of the inputs.
#' @param .path_lib Character. This library.
#' @return Invisibly, the build's status.
oa_build_counts <- function(.path_contracts, .path_orders, .dir_own, .force, .path_lib) {
  if (FALSE) {
    .path_contracts <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "Contracts.parquet"
    )
    .path_orders <- fs::path(fs::path_dir(.path_contracts), "CtoOrders.parquet")
    .dir_own     <- here::here("2_output", "31-OnlineAppendix", "Output")
    .force       <- FALSE
    .path_lib    <- here::here("1_code", "31-OnlineAppendix.R")
  }
  name_ <- "TextCounts"
  outs_ <- fs::path(.dir_own, "Data", paste0(name_, ".parquet"))
  ins_ <- c(.path_contracts, .path_lib, if (fs::file_exists(.path_orders)) .path_orders)
  if (!oa_build_needed(.outputs = outs_, .inputs = ins_, .force = .force)) return(invisible("up to date"))
  fs::dir_create(fs::path(.dir_own, "Data"))
  arrow::write_parquet(
    x    = oa_data_counts(
      .path_contracts = .path_contracts,
      .path_orders    = .path_orders
    ),
    sink = outs_
  )
  invisible("built")
}

#' Report the builds of this appendix's own exhibits, and record them, so a placeholder can say what it waits for
#' @param .status Named character: one status per exhibit.
#' @return Invisibly, the report tibble.
oa_report_builds <- function(.status) {
  if (FALSE) .status <- c(RegulatoryTimeline = "built")
  oa_state_put(
    .what  = "Builds",
    .value = .status
  )
  tab_ <- tibble::tibble(Exhibit = names(.status), Status = unname(.status))
  tbl_say(
    .tab   = tab_,
    .title = "The exhibits this appendix builds"
  )
  wait_ <- tab_$Exhibit[grepl("^waiting", tab_$Status)]
  if (length(wait_) > 0L) {
    cli::cli_alert_warning("Waiting for an input, so set as a placeholder for now: {.val {wait_}}.")
  }
  invisible(tab_)
}
