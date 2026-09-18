# ======================================================================================================================
# _OnlineAppendix.R -- the online appendix's machinery: numbers, floats, references, markers, the LaTeX pass, deployment
# ======================================================================================================================
#
# THE ONLINE APPENDIX IS ONE QMD+R PAIR PER CHAPTER, 40-OnlineAppendix-A onward. Each chapter's text is written once and
# rendered twice: to html, the runbook a reader of the replication site follows, and to a LaTeX fragment the paper's
# Overleaf project inputs, one per chapter. Everything the chapters share sits here, sourced like _Tables.R and
# _Plots.R; a chapter's own library holds only the builders of the exhibits that chapter writes itself.
#
# WHAT IS SHARED: the render state; the numbers a text cites, read from the tibble beside the exhibit that prints them,
# so a sentence and its table cannot disagree; the floats, which point at an exhibit's files by name wherever they were
# written -- 30, the chapter itself, or another chapter; the cross-references; the markers for what is still to write;
# the LaTeX pass and the deployment to the paper folder; and the frame in which a chapter writes its own tables.
#
# THREE KINDS OF FUNCTION. Compute functions return a tibble and print nothing, and report functions print through
# cli and return their input invisibly, as in every library here. EMITTERS are the third kind: they write markup for
# a `results: asis` chunk, or return it for inline R -- LaTeX in the LaTeX pass, markdown in the html pass -- and
# never print through cli, because what they write lands in the appendix itself, where a message would be typeset.
#
# THE HTML PASS SHOWS EACH EXHIBIT IN PLACE. A figure is the writer's png copy; a table is the writer's own tabular,
# converted to html by the Pandoc that Quarto itself runs, after a few normalisations the converter needs (section 4).
# Both passes therefore set the same content, and the html numbers every exhibit as the paper does.
#
# ONE PIECE OF STATE, AND WHY. The text calls an emitter wherever a number, a reference or a float belongs, some forty
# times a chapter. Passing the float registry and the resolved numbers into every one of those calls would put
# bookkeeping into the appendix's own sentences. So Configuration builds the context once, on the page, and stores it
# in .oa_state; the emitters read it there and record what they emitted, which is what Validation checks.
#
# SOURCES. The registry names, for every exhibit, who wrote its files: a key into the chapter's sources table, which
# maps each key to a directory on disk and to the macro the fragment reads it through in Overleaf. "30" is the
# manuscript's exhibits document; a chapter's own key is whatever the chapter calls itself; a chapter that sets an
# exhibit another chapter built registers that chapter as a source too. The response memo's exhibits are registered
# under Section "M": the html pass shows them, the LaTeX pass leaves them out of the fragment.


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
#' @param .dir_data Character. The Data/ directories the tibble may sit in; the first that holds the file is read.
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
#'   hundred, one decimal, no percent sign -- the text writes it), "num1" or "num2" (one or two decimals). "text"
#'   is handled before this function is reached: the value is cited as it is.
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
#' @param .column Character. The column holding the value, or an expression over the tibble's columns, evaluated as
#'   written -- "N / NForm" for a share the tibble holds as two counts.
#' @param .where Character. A filter over the tibble's own columns, evaluated as written.
#' @param .fun Character. "one" requires exactly one matching row; "min", "max", "sum" and "mean" reduce several.
#' @param .format Character. See oa_format_number().
#' @param .dir_data Character. The Data/ directories the exhibit tibble may sit in, the first hit taken.
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
  if (!.column %in% names(tab_)) {
    # AN EXPRESSION, NOT A COLUMN: a share the tibble holds as two counts. Evaluated once, into a column of its own.
    tab_ <- tryCatch(
      dplyr::mutate(tab_, OaValue = !!rlang::parse_expr(.column)),
      error = function(.e) NULL
    )
    if (is.null(tab_)) return(fail_(.why = paste0("no column ", .column, ", and it does not evaluate")))
    .column <- "OaValue"
  }
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
  # A TEXT VALUE -- a version string, a label -- is cited as it is; every other format needs a number.
  if (identical(.format, "text")) {
    if (length(v_) != 1L || is.na(v_) || !nzchar(as.character(v_))) return(fail_(.why = "value missing"))
    return(tibble::tibble(Value = as.character(v_), Status = "ok"))
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
#' @param .dir_data Character. The Data/ directories the exhibit tibbles sit in, 30's first.
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

#' Report the numbers: every value with its exhibit, and the ones that did not resolve
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
  cli::cli_alert_info("A number marked unresolved is a missing exhibit or a changed column at its writer, not a typo here.")
  invisible(.tab)
}


#' The resolved numbers as LaTeX macros
#'
#' The manuscript cites several of the numbers the appendix cites -- the labelled sample, the classifier's scores --
#' and a number typed there is a copy that goes stale at the next re-export. Written here as macros, the manuscript
#' writes \oanum{F1Detailed} and prints what this render resolved. One macro taking a key, rather than one macro per
#' number, because a LaTeX command name cannot contain a digit and half the keys do. A key the file does not hold --
#' a typo, or a number that did not resolve -- prints in bold as ??Key, so it is seen in the PDF rather than lost.
#'
#' @param .tab Tibble from oa_numbers_set().
#' @param .doc Character. The chapter document's stem, named in the file's header.
#' @return Character: the lines of the file.
oa_numbers_tex <- function(.tab, .doc) {
  if (FALSE) {
    .tab <- oa_state_get(.what = "Numbers")
    .doc <- "40-OnlineAppendix-A"
  }
  ok_  <- .tab[.tab$Status == "ok", , drop = FALSE]
  bad_ <- .tab$Key[.tab$Status != "ok"]
  c(
    paste0("% ", strrep("=", 118)),
    paste0("% GENERATED FROM 1_code/", .doc, ".qmd -- DO NOT EDIT THIS FILE. Every number this chapter cites."),
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
#' @param .path Character. The file written; one per chapter, all input by the paper's preamble.
#' @param .doc Character. The chapter document's stem, named in the file's header.
#' @return Invisibly, .path.
oa_numbers_write <- function(.tab, .path, .doc) {
  if (FALSE) {
    .tab  <- oa_state_get(.what = "Numbers")
    .path <- here::here("2_output", "40-OnlineAppendix-A", "Output", "OA-Numbers-A.tex")
    .doc  <- "40-OnlineAppendix-A"
  }
  fs::dir_create(fs::path_dir(.path))
  writeLines(
    text = oa_numbers_tex(
      .tab = .tab,
      .doc = .doc
    ),
    con  = .path
  )
  n_ <- sum(.tab$Status == "ok")
  cli::cli_alert_success("{n_} number{?s} written as \\oanum{{}} macros to {.file {fs::path_file(.path)}}.")
  invisible(.path)
}


# 4. Floats: one table or figure, as LaTeX for Overleaf and as a card in the runbook ----------------------------------

# WHICH TABLES RUN LONGER THAN A PAGE is a chapter's decision, passed to oa_context_set() as .long: such a table is
# written as a longtable, which breaks across pages and repeats its header, and set without a float environment, which
# cannot break. The writer and the float must agree, so the chapter names them once, beside its registry.

#' What each registered exhibit needs on disk, and whether it is there
#'
#' A table needs Tables/<name>.tex and Notes/<name>.tex; a figure needs Figures/<name>.pdf, which the LaTeX pass
#' includes, Figures/<name>.png, which the runbook shows, and Notes/<name>.tex -- under the directory of the exhibit's
#' source. A static table lives in the Overleaf project and not here, so it is listed and cannot be checked -- the one
#' thing a runbook cannot see.
#'
#' The number is the one LaTeX will print -- the section letter and the exhibit's count among the tables, or the
#' figures, of its section -- which holds because the text sets exhibits in the registry's order and Validation
#' checks that it does.
#'
#' @param .floats Tibble: Section, Name, Kind, Source, Caption.
#' @param .static Tibble: Name, File, Note.
#' @param .sources Tibble: Source, Dir, Macro -- where each source's files sit, and the macro that reads them.
#' @param .long Character. The names of the tables written as longtables.
#' @return Tibble: .floats joined to .static, plus Long, Number, Dir, Macro, Needs, nNeed, nHave, Ready and Status.
oa_float_files <- function(.floats, .static, .sources, .long) {
  if (FALSE) {
    .floats  <- tab_floats
    .static  <- tab_static
    .sources <- tab_sources
    .long    <- character(0)
  }
  out_ <- dplyr::left_join(
    .floats,
    .static,
    by           = dplyr::join_by(Name),
    relationship = "one-to-one"
  ) |>
    dplyr::left_join(
      dplyr::select(.sources, "Source", "Dir", "Macro"),
      by = dplyr::join_by(Source)
    )
  needs_ <- purrr::map(seq_len(nrow(out_)), \(.i) {
    n_ <- out_$Name[.i]
    if (!is.na(out_$File[.i])) return(character(0))
    if (identical(out_$Kind[.i], "table")) {
      return(c(paste0("Tables/", n_, ".tex"), paste0("Notes/", n_, ".tex")))
    }
    c(paste0("Figures/", n_, ".pdf"), paste0("Figures/", n_, ".png"), paste0("Notes/", n_, ".tex"))
  })
  have_ <- purrr::map2_int(needs_, out_$Dir, \(.n, .b) sum(fs::file_exists(fs::path(.b, .n))))
  out_ |>
    dplyr::mutate(
      Long   = .data$Name %in% .long,
      Number = paste0(.data$Section, dplyr::row_number()),
      .by    = c("Section", "Kind")
    ) |>
    dplyr::mutate(
      Dir    = as.character(.data$Dir),
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

#' Build the chapter's context, check it, and keep it for the emitters
#'
#' Built once, in Configuration, so a float call in the text names the exhibit and nothing else. The checks are the
#' ones a registry edit can break: a name twice, a kind that is neither table nor figure, a source the sources table
#' does not map, a static row naming an exhibit the registry does not list.
#'
#' @param .floats Tibble: Section, Name, Kind, Source, Caption -- the chapter's exhibits in the order it prints them.
#'   Section "M" marks an exhibit of the response memo, shown in the runbook and left out of the fragment.
#' @param .static Tibble: Name, File, Note -- the exhibits read from the Overleaf project instead of from a source.
#' @param .sources Tibble: Source, Dir, Macro -- every writer the registry names: the key the registry uses, its
#'   output directory (Tables/, Notes/, Figures/, Data/ below it), and the macro the fragment reads it through.
#' @param .own Character. The source key that is this chapter itself.
#' @param .dir_doc Character. The directory this document sits in; the html pass writes image paths relative to
#'   it, because Quarto reads a path starting with a slash as relative to the project root rather than the disk.
#' @param .fig_width Numeric. A figure's width as a share of the text width.
#' @param .strict Logical. TRUE makes a missing exhibit abort the render.
#' @param .long Character. The names of the tables written as longtables; none by default.
#' @return Invisibly, the context: Files, Sources, Own, DirOwn, DirDoc, FigWidth, Strict.
oa_context_set <- function(.floats, .static, .sources, .own, .dir_doc, .fig_width, .strict, .long = character(0)) {
  if (FALSE) {
    .floats    <- tab_floats
    .static    <- tab_static
    .sources   <- tab_sources
    .own       <- "A"
    .dir_doc   <- here::here("1_code")
    .fig_width <- 0.96
    .strict    <- FALSE
    .long      <- character(0)
  }
  miss_ <- setdiff(c("Section", "Name", "Kind", "Source", "Caption"), names(.floats))
  if (length(miss_) > 0L) cli::cli_abort("The float registry lacks {.field {miss_}}.")
  miss_ <- setdiff(c("Name", "File", "Note"), names(.static))
  if (length(miss_) > 0L) cli::cli_abort("The static table lacks {.field {miss_}}.")
  miss_ <- setdiff(c("Source", "Dir", "Macro"), names(.sources))
  if (length(miss_) > 0L) cli::cli_abort("The sources table lacks {.field {miss_}}.")
  dup_ <- unique(.floats$Name[duplicated(.floats$Name)])
  if (length(dup_) > 0L) cli::cli_abort("Registered twice: {.val {dup_}}.")
  kind_ <- setdiff(unique(.floats$Kind), c("table", "figure"))
  if (length(kind_) > 0L) cli::cli_abort("Unknown kind{?s} in the registry: {.val {kind_}}.")
  src_ <- setdiff(unique(.floats$Source), .sources$Source)
  if (length(src_) > 0L) cli::cli_abort("Source{?s} the sources table does not map: {.val {src_}}.")
  if (!.own %in% .sources$Source) cli::cli_abort("{.arg .own} ({.val {(.own)}}) is not in the sources table.")
  stray_ <- setdiff(.static$Name, .floats$Name)
  if (length(stray_) > 0L) cli::cli_abort("Static row{?s} for exhibits not registered: {.val {stray_}}.")
  ctx_ <- list(
    Files    = oa_float_files(
      .floats  = .floats,
      .static  = dplyr::mutate(.static, File = as.character(.data$File), Note = as.character(.data$Note)),
      .sources = .sources,
      .long    = .long
    ),
    Sources  = .sources,
    Own      = .own,
    DirOwn   = as.character(.sources$Dir[.sources$Source == .own]),
    DirDoc   = .dir_doc,
    FigWidth = .fig_width,
    Strict   = .strict
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
#' through its source's macro or \\oastatic, and the note below it. A table is input as the manuscript inputs
#' one of 30's: 30's central writer, fin_tex_write(), gives every table its frame -- rules, row spacing, column widths, and a
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
    "\\\\begin\\{tabular\\*\\}\\{[^}]*\\}\\{"                              = "\\\\begin{tabular}{",
    "@\\{\\\\extracolsep\\{\\\\fill\\}\\}"                                 = "",
    "\\\\end\\{tabular\\*\\}"                                            = "\\\\end{tabular}",
    "\\\\renewcommand\\*?\\{\\\\arraystretch\\}\\{[^}]*\\}"                   = "",
    "\\\\setlength\\{\\\\tabcolsep\\}\\{[^}]*\\}"                        = "",
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
  # THE HEADER BOUNDARY SURVIVES. 31 writes \hline throughout, as the paper's tables do, so the rule between the
  # header and the body arrives here as a second \bottomrule; the html machinery reads a \midrule there.
  rules_pos_ <- gregexpr("\\\\bottomrule", out_, fixed = TRUE)[[1L]]
  if (length(rules_pos_) > 1L && rules_pos_[[1L]] > 0L) {
    out_ <- paste0(
      substr(out_, 1L, rules_pos_[[1L]] - 1L),
      "\\midrule",
      substr(out_, rules_pos_[[1L]] + nchar("\\bottomrule"), nchar(out_))
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
#' @param .ctx List. The chapter's context: DirDoc and Own are used; each exhibit's directory comes with its row.
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
    own_ <- identical(.row$Source, .ctx$Own)
    oa_html_box(.html = if (identical(.row$Status, "incomplete")) {
      writer_ <- if (own_) "this chapter has built " else paste0(.row$Source, " has written ")
      paste0("Incomplete: ", writer_, .row$nHave, " of the ", .row$nNeed, " files it needs (",
             oa_html_escape(.x = .row$Needs), ").")
    } else if (!own_) {
      paste0("Not built yet: ", .row$Source, " would write ", oa_html_escape(.x = .row$Needs), ".")
    } else if (length(why_) == 1L && !is.na(why_) && grepl("^waiting", why_)) {
      paste0("Not built yet: this chapter builds it, and its build is ", oa_html_escape(.x = why_), ".")
    } else {
      "Not built yet: this chapter builds it, and its builder is still to be written."
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
#' the LaTeX pass and the exhibit itself in the html pass, records what it emitted, and refuses to emit one twice. An
#' exhibit of the response memo (Section "M") is recorded and shown in the html pass; in the LaTeX pass it writes
#' nothing, so the fragment carries the appendix alone.
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
    cli::cli_abort("Strict: {.val {(.name)}} is {row_$Status}; {row_$Source} writes {row_$Needs}.")
  }
  oa_state_put(
    .what  = "Emitted",
    .value = c(done_, .name)
  )
  if (oa_is_tex() && identical(row_$Section, "M")) return(invisible(row_))
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
    .title = "The chapter's exhibits, in print order"
  )
  n_ <- table(.files$Status)
  cli::cli_alert_info(
    "{sum(n_)} exhibit{?s}: {paste(names(n_), n_, sep = ' ', collapse = ', ')}."
  )
  cli::cli_alert_info("Not built and incomplete are set as framed placeholders; static is read from the Overleaf project.")
  cli::cli_alert_info("Section M is the response memo: shown here, left out of the fragment.")
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
      "Converted unfaithfully -- leftover text, or words and numbers of the LaTeX missing from the page -- so the \\
       table's writer uses a construct oa_tex_normalise() does not know yet:"
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
  if (FALSE) .path_qmd <- here::here("1_code", "40-OnlineAppendix-A.qmd")
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
  if (FALSE) .tab <- oa_markers(.path_qmd = here::here("1_code", "40-OnlineAppendix-A.qmd"))
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
      "i" = "Render 30-FinalExhibits first; the appendix reads what 30 wrote and never rebuilds it."
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
    .path_tex     <- here::here("_rendered", "1_code", "40-OnlineAppendix-A.tex")
    .path_qmd     <- here::here("1_code", "40-OnlineAppendix-A.qmd")
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
    .path_tex     <- here::here("_rendered", "1_code", "40-OnlineAppendix-A.tex")
    .path_qmd     <- here::here("1_code", "40-OnlineAppendix-A.qmd")
    .dir_exhibits <- here::here("2_output", "30-FinalExhibits", "Output")
  }
  if (!fs::file_exists(.path_tex)) {
    cli::cli_alert_warning(
      "No fragment on disk. Only the LaTeX pass writes {.file {fs::path_file(.path_tex)}}, and the Render button \\
       does not run it."
    )
    cli::cli_alert_info(
      "From the console: {.code oa_publish()}, as shown below. From the terminal: \\
       {.code quarto render {fs::path_rel(.path_qmd, start = here::here())}}, which runs both passes."
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
      "i" = "Pass it as {.arg .quarto}, or run {.code quarto render <chapter>.qmd --to latex} in the terminal."
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
    .path_qmd <- here::here("1_code", "40-OnlineAppendix-A.qmd")
    .path_tex <- here::here("_rendered", "1_code", "40-OnlineAppendix-A.tex")
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
    .a <- here::here("_rendered", "1_code", "40-OnlineAppendix-A.tex")
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
#' Run from the console, or by oa_publish(). The folder is the appendix's own, shared by every chapter and beside 30's
#' current/ rather than inside it: 30's deployment moves current/ to its archive wholesale and would take these files
#' with it. The exhibits a chapter builds itself travel with its fragment, in the same Tables/, Notes/ and Figures/
#' layout, since the fragment reads them through the chapter's own macro from the same folder in Overleaf; exhibit
#' names are unique across chapters, so the chapters' files do not collide there. A stale fragment or numbers file is
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
    .path_tex     <- here::here("_rendered", "1_code", "40-OnlineAppendix-A.tex")
    .path_numbers <- here::here("2_output", "40-OnlineAppendix-A", "Output", "OA-Numbers-A.tex")
    .path_qmd     <- here::here("1_code", "40-OnlineAppendix-A.qmd")
    .dir_exhibits <- here::here("2_output", "30-FinalExhibits", "Output")
    .dir_own      <- here::here("2_output", "40-OnlineAppendix-A", "Output")
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
    .path_qmd     <- here::here("1_code", "40-OnlineAppendix-A.qmd")
    .path_tex     <- here::here("_rendered", "1_code", "40-OnlineAppendix-A.tex")
    .path_numbers <- here::here("2_output", "40-OnlineAppendix-A", "Output", "OA-Numbers-A.tex")
    .dir_exhibits <- here::here("2_output", "30-FinalExhibits", "Output")
    .dir_own      <- here::here("2_output", "40-OnlineAppendix-A", "Output")
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


# 8. Build helpers: what every chapter's own exhibits share ----------------------------------------------------------
# The exhibits a chapter builds itself are written in 30's layout -- Tables/, Notes/, Figures/, Data/ -- under the
# chapter's own output directory, and the fragment reads them through the macro the chapter registers for itself. A
# build runs when its inputs change: it compares its outputs with its inputs and with the chapter's library, and runs
# only when an output is missing or older; Rebuild forces every build.
#
# THE TABLE FRAME MIRRORS 30'S. fin_tex_write() lives in 30's library, which no chapter sources. The frame is repeated
# here -- \arraystretch 1.5, \hline\hline on top, \midrule under the header, \hline at the bottom, widths by
# \dimexpr, no type size -- and moves back to one writer when an exhibit moves to 30.

#' Whether a build has to run
#'
#' @param .outputs Character. The files the build writes.
#' @param .inputs Character. Files and directories it reads; a directory stands for its newest file.
#' @param .force Logical. TRUE runs the build regardless.
#' @return Logical scalar.
oa_build_needed <- function(.outputs, .inputs, .force) {
  if (FALSE) {
    .outputs <- fs::path_temp("x.tex")
    .inputs  <- here::here("1_code", "40-OnlineAppendix-A.R")
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
#' @param .dir_own Character. The chapter's output directory.
#' @return Invisibly, the paths written.
oa_write_exhibit <- function(.name, .lines, .note, .data, .dir_own) {
  if (FALSE) {
    .name    <- "FormCoverage"
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

#' Report the builds of a chapter's own exhibits, and record them, so a placeholder can say what it waits for
#' @param .status Named character: one status per exhibit.
#' @return Invisibly, the report tibble.
oa_report_builds <- function(.status) {
  if (FALSE) .status <- c(FormCoverage = "built")
  oa_state_put(
    .what  = "Builds",
    .value = .status
  )
  tab_ <- tibble::tibble(Exhibit = names(.status), Status = unname(.status))
  tbl_say(
    .tab   = tab_,
    .title = "The exhibits this chapter builds"
  )
  wait_ <- tab_$Exhibit[grepl("^waiting", tab_$Status)]
  if (length(wait_) > 0L) {
    cli::cli_alert_warning("Waiting for an input, so set as a placeholder for now: {.val {wait_}}.")
  }
  invisible(tab_)
}
