# ======================================================================================================================
# _NER.R -- entity extraction: three families, one entry point, one database each
# ======================================================================================================================
#
# WHAT THIS FILE IS
# Everything that talks to an extractor or to a candidate store. 04A drives it over the labelled
# sample; 04C drives the same functions over the corpus, which is what makes the two stores
# comparable rather than merely similar.
#
# THE THREE FAMILIES, AND WHY THE WORD
#
#   lexnlp   one container, one version, four entities from one read of the text
#   matcon   one CLI, four extractor modules, each with its OWN version tag
#   spacy    four models, each producing the same three entities
#
# A family is a piece of software with its own way of being installed, versioned and called. It is
# the unit that has an interpreter or an image, and therefore the unit a benchmark describes.
#
# ONE DATABASE PER FAMILY, ONE TABLE PER ENTITY
# The previous design put every family in one file with Engine and Model columns on every span row,
# and a combination token -- "paper:gazetteer-v1" -- that callers typed by hand. Three of the four
# matcon versions have since moved, so every one of those typed tokens is now wrong, and the failure
# is silent: the query matches nothing, returns an empty tibble, and the document renders reporting
# zero spans as a success.
#
# Splitting by family turns that class of mistake into a missing file. A wrong family is a wrong
# path, and paths fail loudly. It also removes the shared schema: each entity table carries exactly
# the columns its own family emits, with no NULL padding and nothing to negotiate. Cross-family work
# is DuckDB's ATTACH, which costs one line and copies nothing.
#
# THE SCHEMA IS DERIVED, NOT DECLARED
# Entity tables are created from the staged parquet's own schema. A hand-written column list that
# claims to describe a file it never reads is the relocate(any_of(...)) failure in another costume:
# it is neither a superset nor a subset of what is actually there, and no inference about presence
# can be drawn from it in either direction. Reading the schema is the only statement about it.
#
# The consequence worth knowing: Amount arrives as text and stays as text. moneyregex crosses the
# seam as a string precisely so a contract value survives intact, and the cast belongs wherever
# somebody does arithmetic, not here.
#
# TWO MECHANISMS, TWO FAILURES
#   Declared version bump   the ledger catches it, because Model is part of its key. matcon only.
#   Undeclared change       a pattern edited without a version bump, an image rebuilt, a spaCy model
#                           upgraded in place -- only the manifest catches these, for all three.
# A manifest mismatch reports and aborts. The same function serves the corpus pass, where
# re-extracting because a hash moved is days of work, and that is a decision rather than a side
# effect.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new columns; if (FALSE) dev blocks;
# cli/fs/here; pure ASCII; stringi::stri_sub never base substr; {(.arg)} parens in cli interpolation.


# 0. Where things live -------------------------------------------------------------------------------------------------

#' Default worker count
#'
#' Five, and it is the same number the benchmark uses, so the ranking a benchmark reports is the
#' ranking that applies to the pass. Sweeping workers as well as batch was considered and dropped:
#' it doubles the grid to answer a question nobody has, and a batch ranking measured at one worker
#' count and applied at another is an assumption this pipeline has made silently before.
.ner_workers <- 5L

#' Default documents per unit of work
.ner_batch_size <- 32L

#' Default per-document cap, seconds
#'
#' A HANG DETECTOR, NOT A BUDGET. At matcon's throughput a document taking ten minutes is running
#' five orders of magnitude beyond the median and is pathological by definition.
#'
#' WHERE LEXNLP ACTUALLY SPENDS ITS TIME, measured rather than assumed. An earlier note here -- and
#' in 04A's prose -- said the geoentity pass cost about five seconds a document and was the
#' bottleneck. build_geo_locator()'s own docstring records the measurement that superseded it: on a
#' 300-document draw the geoentity pass runs at 64 documents a second against 4.9 for ORG, DATE and
#' MONEY together, so geoentities are roughly six per cent of the bill. The expensive work is the
#' maxent company NER and the date grammar.
#'
#' Two consequences. Dropping GPE from LexNLP's request would save almost nothing, so asking for it
#' is nearly free rather than a cost decision. And the generous cap is still right, but for the ORG
#' and DATE extractors rather than the geo one -- a timeout there is not one lost document but
#' documents lost SYSTEMATICALLY IN THE LONGEST AGREEMENTS, which is the worst possible place for a
#' missing-at-random assumption to fail.
.ner_timeout <- 600L

#' The entities each third-party family produces
#'
#' Declared because neither can say. matcon describes itself and is absent from this list.
#'
#' PERSON IS NOT ON LEXNLP'S ROW, and that is a fact about the library rather than a policy: its
#' person extractor has no offset API, so a span it found could not be located in the text and the
#' offset contract could not hold for it.
.ner_declared <- list(
  lexnlp = c("ORG", "GPE", "DATE", "MONEY"),
  spacy  = c("ORG", "PERSON", "GPE")
)

#' The three families, in the order a pass should run them
#'
#' Cheapest first, so a render interrupted in the expensive family still leaves two complete.
.ner_families <- c("matcon", "spacy", "lexnlp")

#' Where each family lives
#'
#' Resolved from the repository root, so a checkout anywhere works and no home directory is written
#' down. Each family is its own uv project and is invoked through its own interpreter: never
#' `uv run` from the repository root, which resolves the root environment instead of the family's.
#'
#' @param .family One of "lexnlp", "matcon", "spacy".
#' @return Path to the family's folder.
.ner_family_dir <- function(.family) {
  if (FALSE) {
    .family <- "matcon"
  }
  here::here(switch(
    .family,
    spacy  = "contracts-spacy",
    lexnlp = "contracts-lexnlp",
    matcon = "contracts-extract",
    cli::cli_abort("Unknown family {(.family)}.")
  ))
}

#' A family's Python interpreter
#'
#' @param .family One of "matcon", "spacy". LexNLP has no interpreter on the host; it runs in a
#'   container.
#' @return Path to the family venv's python.
.ner_python <- function(.family) {
  if (FALSE) {
    .family <- "matcon"
  }
  out_ <- fs::path(.ner_family_dir(.family), ".venv", "bin", "python")
  if (!fs::file_exists(out_)) {
    cli::cli_abort(c(
      "No interpreter for family {(.family)} at {(out_)}.",
      "i" = "cd {(.ner_family_dir(.family))} && uv venv --python 3.12 && uv pip install -e ."
    ))
  }
  out_
}

#' Which device a spaCy model should run on
#'
#' THE ONLY SURVIVING PER-MODEL TUNING VALUE, and it is not really tuning -- it is a fact about what
#' each pipeline is made of.
#'
#' Activating a GPU forces n_process to 1, because one Metal device cannot be shared across worker
#' processes. For the transformer that is the right trade: the model is a single large matrix
#' operation per batch and the device wins by more than the workers it costs.
#'
#' For the CNN pipelines it is exactly the wrong trade. Their per-document work is small enough that
#' host-device transfer dominates the arithmetic, so activating the GPU buys almost nothing and pays
#' for it by dropping to one worker.
#'
#' @param .model spaCy model name.
#' @return "auto" for a transformer pipeline, "cpu" otherwise.
.ner_spacy_device <- function(.model) {
  if (FALSE) {
    .model <- "en_core_web_trf"
  }
  if (stringi::stri_detect_fixed(.model, "trf")) "auto" else "cpu"
}

#' Run a subprocess, stream its output, and abort on failure
#'
#' Every family call goes through this. Output streams to the console rather than being captured,
#' because these run for minutes to hours and a progress bar nobody can see is worse than none. A
#' non-zero exit aborts rather than returning quietly: a family that failed halfway leaves a partial
#' parquet, and ingesting it would record partial extraction as complete.
#'
#' @param .cmd Executable.
#' @param .args Character vector of arguments.
#' @param .label What to name in the abort message.
#' @return Elapsed seconds, invisibly.
.ner_system <- function(.cmd, .args, .label) {
  if (FALSE) {
    .cmd   <- "echo"
    .args  <- "hello"
    .label <- "demo"
  }
  t0_     <- Sys.time()
  status_ <- system2(.cmd, .args, stdout = "", stderr = "")
  secs_   <- as.numeric(difftime(Sys.time(), t0_, units = "secs"))
  if (!identical(status_, 0L)) {
    cli::cli_abort("{(.label)} exited with status {(status_)} after {round(secs_, 1)}s.")
  }
  invisible(secs_)
}


# 1. What each family can produce ---------------------------------------------------------------------------------------
#
# WHAT THIS SECTION SOLVES. The ledger is keyed on (DocID, Model, Entity), so R must know the model
# tag BEFORE a pass in order to ask what still needs doing. But the tag is stamped by the extractor
# and never typed in R -- that is the rule that keeps a version string out of the runbooks.
# Something has to bridge the two.
#
# The previous answer was a table in R naming each extractor's labels, keyed on the module STEM. It
# was silently wrong the moment dateregex-v3 emitted a second label: the stem said DATE, the request
# asked for DATE, TERM was filtered out inside Python, and the ledger recorded success. Nothing
# errored, and no check could have caught it.
#
# So our own package answers for itself. THE ASYMMETRY IS DELIBERATE: spaCy and LexNLP are declared
# because we did not write them, and their declarations are checked against what actually arrives.

#' Which spaCy models are installed, and at what version
#'
#' A MODEL NAME IS NOT A MODEL. Declaring four and having three is not an error anyone notices until
#' the fourth is loaded, which on this pipeline is after the other three have finished -- and the
#' third of them is the slow one.
#'
#' The version comes back with the name because spaCy reports it nowhere else: extract_spacy.py
#' prints no version, so without this the manifest would have nothing to record and a model upgraded
#' in place would go undetected.
#'
#' @return Tibble: Model, Version. Empty if the suite cannot be reached.
ner_spacy_installed <- function() {
  if (FALSE) {
    # no arguments
  }

  code_ <- paste(
    "import spacy, json, importlib.metadata as md;",
    "print(json.dumps({m: md.version(m) for m in sorted(spacy.util.get_installed_models())}))"
  )
  raw_ <- suppressWarnings(system2(
    .ner_python("spacy"), c("-c", shQuote(code_)), stdout = TRUE, stderr = FALSE
  ))
  if (length(raw_) == 0L || !is.null(attr(raw_, "status"))) {
    cli::cli_warn("Could not ask the spaCy family which models it has; assuming none.")
    return(tibble::tibble(Model = character(0), Version = character(0),
                          SpecHash = character(0)))
  }

  lst_ <- jsonlite::fromJSON(paste(raw_, collapse = ""), simplifyVector = FALSE)
  if (length(lst_) == 0L) return(tibble::tibble(Model = character(0), Version = character(0),
                          SpecHash = character(0)))

  tibble::tibble(Model = names(lst_), Version = as.character(unlist(lst_))) |>
    dplyr::mutate(SpecHash = ner_spacy_spec(.model = .data$Model, .version = .data$Version))
}

#' The spaCy family's spec hash -- the script AND the model, together
#'
#' H5, AND IT WAS AN IDENTITY DEFECT RATHER THAN A COSMETIC ONE. ner_describe() set this family's
#' SpecHash to the MODEL VERSION -- 3.8.0 -- while matcon and lexnlp both reported a 12-character
#' content hash. Three consequences followed from that one line:
#'
#'   EDITING extract_spacy.py MOVED NOTHING. The script decides which columns are emitted and how
#'   offsets are shifted across windows, and none of it touched the identity. ner_manifest_write()
#'   would have admitted rows from the edited script beside rows from the old one under an unchanged
#'   tag, and the store would hold two generations with nothing able to tell them apart. That is
#'   exactly the failure the manifest exists to catch, and it was unreachable for this family.
#'
#'   THE COLUMN HELD TWO DIFFERENT KINDS OF THING. A hash on two families and a version string on
#'   the third, so any comparison across families was comparing a fingerprint with a label.
#'
#'   AND IT BLOCKED THE SCHEMA. The cue columns could not be added to spaCy until this was fixed,
#'   because adding them is precisely an edit whose identity would not have moved.
#'
#' BOTH INPUTS MATTER, SO BOTH ARE HASHED. The model determines which spans are found; the script
#' determines what is emitted about them and how the window offsets are resolved. A hash over either
#' alone leaves the other free to change silently.
#'
#' @param .model Model name, vectorised.
#' @param .version Installed version of that model, vectorised alongside.
#' @return Character vector of 12-character hashes, one per model.
ner_spacy_spec <- function(.model, .version) {
  if (FALSE) {
    .model   <- "en_core_web_trf"
    .version <- "3.8.0"
  }

  path_ <- fs::path(.ner_family_dir("spacy"), "extract_spacy.py")
  # ABSENT IS ITS OWN VALUE, NOT AN ERROR. A missing script is a real state during a partial
  # install, and it must hash to something stable and obviously wrong rather than abort a describe
  # that is being run precisely to find out what is missing.
  file_ <- if (fs::file_exists(path_)) {
    substr(digest::digest(file = path_, algo = "sha256"), 1L, 12L)
  } else {
    "absent"
  }

  purrr::map2_chr(.model, .version, \(.m, .v) {
    substr(
      digest::digest(list(Script = file_, Model = .m, Version = .v), algo = "sha256"), 1L, 12L
    )
  })
}

#' Is the LexNLP container usable, and does it match the sources it was built from?
#'
#' The image is built by hand and used for months, and nothing inside it records which version of
#' extract_lexnlp.py, company_types.csv or geoentities.csv it holds. An edit to any of them leaves a
#' running container that no longer matches the repository, and the extraction it produces is
#' attributed in the store to code that has changed since. The container runs, the parquet is well
#' formed, and the rows are wrong about their own provenance. image_spec.py hashes those four files
#' and stamps the result into the image as a build label, so this reads it back.
#'
#' SIX STATES, BECAUSE THE REMEDIES DIFFER. image_spec.py collapses "no docker", "no image" and "no
#' label" into one exit code, deliberately, because from its point of view all three mean rebuild.
#' From here they do not: a daemon that is not running is fixed by starting Docker, and an image that
#' is present but unlabelled runs perfectly well. A check whose failure does not say what to do costs
#' a diagnosis every time.
#'
#'   ok          daemon up, image present, its label matches the sources
#'   stale       daemon up, image present, label absent or different -- runnable, attribution wrong
#'   unverified  daemon up, image present and labelled, but the sources could not be hashed
#'   noimage     daemon up, no image of that name
#'   nodaemon    docker on PATH, daemon not answering
#'   nodocker    docker not on PATH
#'
#' EACH STATE IS ESTABLISHED BY THE SIMPLEST CALL THAT SETTLES IT, and stderr is kept rather than
#' discarded. Existence comes from `image ls -q`, which needs no template and behaves the same under
#' both of Docker's image stores; the earlier version asked `inspect --format '{{.Id}}'` and broke on
#' Docker 29, where the containerd store formats against a struct whose field is ID -- a template
#' error that was read as "image absent" while `docker image ls` listed the image happily. Whatever
#' docker says is carried into `Said`, so the next unfamiliar failure names itself instead of being
#' inferred from an exit code.
#'
#' SPECHASH IS THE IMAGE'S OWN LABEL, NOT THE SOURCES' HASH. The manifest records what produced the
#' rows, and for a stale image that is the label the container carries. Recording the source hash
#' instead would make a stale image indistinguishable from a current one and defeat the whole
#' mechanism: a later render with a rebuilt image would see an unchanged hash and say nothing.
#'
#' @param .image Docker image name.
#' @return Tibble: State, SpecHash (the image's label, or NA), Said (what was learned).
ner_lexnlp_image <- function(.image = "contracts-lexnlp") {
  if (FALSE) {
    .image <- "contracts-lexnlp"
  }

  out_ <- \(.state, .hash, .said) {
    tibble::tibble(State = .state, SpecHash = .hash, Said = .said)
  }

  # THE BINARY IS NOT THE DAEMON. An R session started from the GUI routinely has a narrower PATH
  # than a login shell, so a missing binary is usually a PATH problem; a present binary with a dead
  # daemon is usually Docker Desktop not being open. Two different sentences.
  if (Sys.which("docker") == "") {
    return(out_("nodocker", NA_character_, "docker is not on this session's PATH"))
  }
  if (!identical(suppressWarnings(
    system2("docker", "info", stdout = FALSE, stderr = FALSE)
  ), 0L)) {
    return(out_("nodaemon", NA_character_, "docker is on PATH but the daemon is not answering"))
  }

  # EXISTENCE IS NOT A TEMPLATE QUESTION, and asking it as one broke on Docker 29. Under the
  # containerd image store `docker image inspect` formats against a typed struct whose field is ID,
  # so `--format '{{.Id}}'` raises a template error -- which the first version read as "image
  # absent" and reported as noimage while `docker image ls` listed the image happily.
  #
  # `image ls -q` prints identifiers and nothing else, needs no template, and behaves the same under
  # both image stores. A question that can be asked without a template should be.
  id_ <- suppressWarnings(system2(
    "docker", c("image", "ls", "-q", .image), stdout = TRUE, stderr = TRUE
  ))
  if (!is.null(attr(id_, "status")) || length(id_) == 0L || !any(nzchar(trimws(id_)))) {
    return(out_("noimage", NA_character_, paste0(
      "no image named ", .image,
      if (length(id_) > 0L) paste0(" -- docker said: ", paste(id_, collapse = " ")) else ""
    )))
  }

  # STDERR IS KEPT, NOT DISCARDED. An empty label and a template that could not be evaluated are
  # different findings with different remedies, and an exit code cannot tell them apart. Three
  # diagnoses in this pair have been lost to a thrown-away error stream.
  fmt_ <- "{{ with .Config.Labels }}{{ index . \"spec_hash\" }}{{ end }}"
  lab_ <- suppressWarnings(system2(
    "docker", c("image", "inspect", .image, "--format", shQuote(fmt_)),
    stdout = TRUE, stderr = TRUE
  ))
  said_ <- trimws(paste(lab_, collapse = " "))
  have_ <- if (!is.null(attr(lab_, "status")) || length(lab_) == 0L) {
    NA_character_
  } else {
    trimws(paste(lab_, collapse = ""))
  }
  if (!is.na(have_) && !nzchar(have_)) have_ <- NA_character_

  script_ <- fs::path(.ner_family_dir("lexnlp"), "image_spec.py")
  if (!fs::file_exists(script_)) {
    return(out_("unverified", have_, "image_spec.py is absent, so the sources cannot be hashed"))
  }
  want_ <- suppressWarnings(system2("python3", script_, stdout = TRUE, stderr = FALSE))
  if (!is.null(attr(want_, "status")) || length(want_) == 0L) {
    return(out_("unverified", have_, "image_spec.py could not hash the sources"))
  }
  want_ <- trimws(paste(want_, collapse = ""))

  if (is.na(have_)) {
    return(out_("stale", have_, paste0(
      "the image carries no readable spec_hash label",
      if (nzchar(said_)) paste0(" -- docker said: ", said_) else "; it predates the check"
    )))
  }
  if (!identical(have_, want_)) {
    return(out_("stale", have_, paste0("image=", have_, " sources=", want_)))
  }
  out_("ok", have_, paste0("current (", have_, ")"))
}

#' What matcon-extract will stamp, asked of the package itself
#'
#' Calls `python -m matcon_extract --describe` and parses the JSON. One subprocess, roughly a second,
#' and it replaces every R-side statement of what matcon can do.
#'
#' `SpecHash` fingerprints the constants that determine an extractor's output -- pattern tables,
#' vocabularies, and for the gazetteer a content hash of its 116,000-entry lookup. A hash that has
#' moved under an unchanged model tag means somebody edited a rule without bumping the version, which
#' would silently change what an existing store's rows mean.
#'
#' `Ready` is FALSE where a data dependency is absent. The gazetteer without its lookup can produce
#' no output at all, so it reports no hash rather than one that merely looks valid.
#'
#' @return Tibble: Module, Model, Entities (list), Extras (list), SpecHash, Ready.
ner_matcon_describe <- function() {
  if (FALSE) {
    # no arguments
  }

  raw_ <- system2(.ner_python("matcon"), c("-m", "matcon_extract", "--describe"),
                  stdout = TRUE, stderr = FALSE)
  if (length(raw_) == 0L) cli::cli_abort("matcon-extract --describe returned nothing.")

  lst_ <- jsonlite::fromJSON(paste(raw_, collapse = "\n"), simplifyVector = FALSE)

  out_ <- lst_ |>
    purrr::keep(\(.d) isTRUE(.d$available)) |>
    purrr::map(\(.d) tibble::tibble(
      Module   = .d$module,
      Model    = .d$model,
      Entities = list(unlist(.d$labels)),
      Extras   = list(unlist(.d$extras)),
      SpecHash = if (is.null(.d$spec_hash)) NA_character_ else .d$spec_hash,
      Ready    = isTRUE(.d$ready)
    )) |>
    purrr::list_rbind()

  if (nrow(out_) == 0L) cli::cli_abort("matcon-extract reports no installed extractors.")
  out_
}

#' Every family, model and entity in one table
#'
#' ONE ROW PER (Family, Model, Entity). This is the grid the ledger is queried against and the
#' manifest fingerprints, so a document processed for ORG and not for GPE is two rows with two
#' independent outcomes rather than one ambiguous one.
#'
#' `Ready` is FALSE for two recoverable reasons -- a spaCy model declared but not downloaded, or a
#' matcon extractor whose data dependency is absent. Neither stops a pass; both are named, because a
#' comparison across three models where four were intended is a different result and nothing else
#' would record the difference.
#'
#' NOTE CARRIES THE REASON, so a reader is never told only that something is unavailable. The three
#' causes have three different remedies -- start Docker, rebuild the image, download a model -- and a
#' report that names the state without naming the fix costs a diagnosis every time it fires.
#'
#' @param .spacy_models spaCy model names to describe.
#' @param .lexnlp_image Docker image name for the LexNLP family.
#' @return Tibble: Family, Model, Entity, SpecHash, Ready, Note.
ner_describe <- function(.spacy_models = c("en_core_web_sm", "en_core_web_md",
                                           "en_core_web_lg", "en_core_web_trf"),
                         .lexnlp_image = "contracts-lexnlp") {
  if (FALSE) {
    .spacy_models <- c("en_core_web_lg", "en_core_web_trf")
    .lexnlp_image <- "contracts-lexnlp"
  }

  mat_ <- ner_matcon_describe() |>
    dplyr::mutate(Family = "matcon") |>
    tidyr::unnest_longer(col = "Entities", values_to = "Entity") |>
    dplyr::mutate(Note = dplyr::if_else(.data$Ready, "", "data dependency absent")) |>
    dplyr::select("Family", "Model", "Entity", "SpecHash", "Ready", "Note")

  # READINESS IS MEASURED, NOT DECLARED. This was TRUE by construction in the first version, so the
  # report announced that every family was ready while the Docker daemon was down and the benchmark
  # aborted three chunks later. A claim that cannot fail is not a check.
  img_  <- ner_lexnlp_image(.image = .lexnlp_image)
  note_ <- switch(
    img_$State[[1L]],
    ok         = "",
    stale      = paste0("image does not match its sources (", img_$Said[[1L]],
                        "); run contracts-lexnlp/rebuild_lexnlp.sh"),
    unverified = paste0("running, but unverified: ", img_$Said[[1L]]),
    noimage    = "no image; run contracts-lexnlp/rebuild_lexnlp.sh",
    nodaemon   = "the Docker daemon is not running; start Docker Desktop",
    nodocker   = "docker is not on this session's PATH"
  )
  lex_ <- tidyr::expand_grid(
    Family = "lexnlp",
    Model  = "lexnlp",
    Entity = .ner_declared$lexnlp
  ) |>
    dplyr::mutate(
      SpecHash = img_$SpecHash[[1L]],
      Ready    = img_$State[[1L]] %in% c("ok", "stale", "unverified"),
      Note     = note_
    )

  have_ <- ner_spacy_installed()
  spa_  <- tidyr::expand_grid(Model = .spacy_models, Entity = .ner_declared$spacy) |>
    dplyr::mutate(Family = "spacy") |>
    dplyr::left_join(have_, by = dplyr::join_by(Model)) |>
    dplyr::transmute(
      .data$Family, .data$Model, .data$Entity,
      # H5: A CONTENT HASH, LIKE EVERY OTHER FAMILY. This column used to carry the model version
      # here and a 12-character hash on matcon and lexnlp rows -- two different kinds of thing under
      # one name. ner_spacy_spec() folds the script and the model together, so an edit to either
      # moves it and a comparison across families compares like with like. The version has not been
      # lost; it moved to Note, where it is metadata rather than identity.
      .data$SpecHash,
      Ready    = !is.na(.data$Version),
      Note     = dplyr::if_else(
        is.na(.data$Version), "not installed; spacy download it", paste0("model ", .data$Version)
      )
    )

  dplyr::bind_rows(mat_, lex_, spa_) |>
    dplyr::arrange(.data$Family, .data$Model, .data$Entity)
}

#' Is this family, or this model, usable right now?
#'
#' @param .describe Output of ner_describe().
#' @param .family Family name.
#' @param .model Model name, or NULL for the whole family.
#' @return TRUE where every relevant row is ready.
ner_ready <- function(.describe, .family, .model = NULL) {
  if (FALSE) {
    .describe <- tab_describe
    .family   <- "lexnlp"
    .model    <- NULL
  }
  rows_ <- dplyr::filter(.describe, .data$Family == .family)
  if (!is.null(.model)) rows_ <- dplyr::filter(rows_, .data$Model == .model)
  nrow(rows_) > 0L && all(rows_$Ready)
}

#' Why a family or model is not usable, in one sentence
#'
#' @param .describe Output of ner_describe().
#' @param .family Family name.
#' @param .model Model name, or NULL for the whole family.
#' @return A single string; empty where everything is ready.
ner_why_not <- function(.describe, .family, .model = NULL) {
  if (FALSE) {
    .describe <- tab_describe
    .family   <- "lexnlp"
    .model    <- NULL
  }
  rows_ <- dplyr::filter(.describe, .data$Family == .family, !.data$Ready)
  if (!is.null(.model)) rows_ <- dplyr::filter(rows_, .data$Model == .model)
  paste(unique(rows_$Note[nchar(rows_$Note) > 0L]), collapse = "; ")
}

#' Which entities a family can produce
#'
#' @param .describe Output of ner_describe().
#' @param .family Family name.
#' @return Character vector of entity names.
ner_entities <- function(.describe, .family) {
  if (FALSE) {
    .describe <- ner_describe()
    .family   <- "matcon"
  }
  sort(unique(.describe$Entity[.describe$Family == .family]))
}

#' Refuse an entity a family cannot produce, before a subprocess starts
#'
#' Asking spaCy for MONEY currently costs a full pass that quietly produces nothing: the label is
#' filtered out inside Python, the parquet is well formed, and the ledger records success. One second
#' here beats discovering it afterwards.
#'
#' @param .describe Output of ner_describe().
#' @param .family Family name.
#' @param .entity Entities requested.
#' @return .entity, invisibly.
ner_check_entities <- function(.describe, .family, .entity) {
  if (FALSE) {
    .describe <- ner_describe()
    .family   <- "spacy"
    .entity   <- c("ORG", "MONEY")
  }
  can_ <- ner_entities(.describe = .describe, .family = .family)
  bad_ <- setdiff(.entity, can_)
  if (length(bad_) > 0L) {
    cli::cli_abort(c(
      "{(.family)} cannot produce {paste(bad_, collapse = ', ')}.",
      "i" = "It produces: {paste(can_, collapse = ', ')}."
    ))
  }
  invisible(.entity)
}

#' Report the description, naming what is not available
#'
#' @param .describe Output of ner_describe().
#' @return .describe, invisibly.
ner_report_describe <- function(.describe) {
  if (FALSE) {
    .describe <- ner_describe()
  }

  wide_ <- .describe |>
    dplyr::summarise(
      Entities = paste(sort(unique(.data$Entity)), collapse = ", "),
      SpecHash = dplyr::first(.data$SpecHash),
      Ready    = dplyr::first(.data$Ready),
      .by      = c("Family", "Model")
    ) |>
    dplyr::arrange(.data$Family, .data$Model)

  tbl_say(.tab = wide_, .title = "What each family and model produces")

  bad_ <- .describe |>
    dplyr::filter(!.data$Ready) |>
    dplyr::summarise(Why = dplyr::first(.data$Note), .by = c("Family", "Model"))

  if (nrow(bad_) > 0L) {
    tbl_say(.tab = bad_, .title = "NOT AVAILABLE, and why")
    cli::cli_alert_warning(
      "{nrow(bad_)} model{?s} {?is/are} excluded from every section below. \\
       Fix the cause and re-render: the ledger fills in what is missing and re-runs nothing else."
    )
  } else {
    cli::cli_alert_success("Every declared model is installed and ready.")
  }

  stale_ <- .describe |>
    dplyr::filter(.data$Ready, nchar(.data$Note) > 0L) |>
    dplyr::distinct(.data$Model, .data$Note)
  if (nrow(stale_) > 0L) {
    tbl_say(.tab = stale_, .title = "RUNNABLE, but not as declared")
    cli::cli_alert_warning(
      "Extraction from these is attributed in the store to code that has changed since."
    )
  }

  invisible(.describe)
}


# 2. Extraction --------------------------------------------------------------------------------------------------------
#
# ONE ENTRY POINT, THREE IMPLEMENTATIONS BEHIND IT. The families differ in how they are invoked --
# a container, a console script, a venv interpreter -- and in nothing a caller should have to know.
#
# .entity IS A VECTOR AND THAT IS A COST DECISION, not a convenience. One spaCy pass yields ORG,
# PERSON and GPE together because they are the same forward pass; one LexNLP container run reads the
# document once for all four of its entities. Looping over entities instead would read every
# document four times through the most expensive family in the pipeline.
#
# EXTRACTION WRITES FILES AND TOUCHES NO DATABASE. That is what makes the benchmark possible without
# an .ingest switch somebody has to remember to set: a timing run and a real run call the same
# function, and only the caller differs.

#' Extract with LexNLP, in its container
#'
#' @param .path_in Staged parquet carrying .id_col and .text_col.
#' @param .out_dir Directory for the output parquet.
#' @param .entity Entities to request.
#' @param .id_col,.text_col Column names in the staged parquet.
#' @param .max_chars Truncate each document to its first N characters. 0 disables.
#' @param .workers Worker processes.
#' @param .batch_size Documents per task.
#' @param .timeout Per-document cap in seconds.
#' @param .image Docker image name.
#' @param .quiet Suppress the progress bar.
#' @return Path written, with an "elapsed" attribute.
.ner_run_lexnlp <- function(.path_in, .out_dir, .entity, .id_col, .text_col, .max_chars,
                            .workers, .batch_size, .timeout, .image, .quiet) {
  if (FALSE) {
    .path_in    <- .lP$Output$Sample
    .out_dir    <- .lP$Output$Stage
    .entity     <- c("ORG", "GPE", "DATE", "MONEY")
    .id_col     <- "DocID"
    .text_col   <- "TextRaw"
    .max_chars  <- 0L
    .workers    <- 5L
    .batch_size <- 8L
    .timeout    <- 240L
    .image      <- "contracts-lexnlp"
    .quiet      <- FALSE
  }

  # THE BINARY IS NOT THE DAEMON, and the first version tested the wrong one. `docker` was on PATH
  # with Docker Desktop closed, so this guard passed and the failure arrived from the container as
  # "exited with status 1 after 0.1s" -- true, and useless.
  img_ <- ner_lexnlp_image(.image = .image)
  if (img_$State[[1L]] %in% c("nodocker", "nodaemon", "noimage")) {
    cli::cli_abort(c(
      "The LexNLP family cannot run: {(img_$Said[[1L]])}.",
      "i" = switch(
        img_$State[[1L]],
        nodocker = "Install Docker, or start R from a shell whose PATH carries it.",
        nodaemon = "Start Docker Desktop and re-render.",
        noimage  = "Build it with contracts-lexnlp/rebuild_lexnlp.sh."
      )
    ))
  }
  fs::dir_create(.out_dir)

  in_   <- fs::path_real(.path_in)
  base_ <- fs::path_dir(in_)
  out_  <- fs::path(.out_dir, "lexnlp__lexnlp.parquet")

  args_ <- c(
    "run", "--rm",
    "-v", paste0(as.character(base_), ":/work:ro"),
    "-v", paste0(as.character(fs::path_real(.out_dir)), ":/out"),
    .image,
    fs::path("/work", fs::path_file(in_)),
    "--output",     fs::path("/out", fs::path_file(out_)),
    "--id-col",     .id_col,
    "--text-col",   .text_col,
    "--label",      .entity,
    "--max-chars",  .max_chars,
    "--n-process",  .workers,
    "--chunk-size", .batch_size,
    "--timeout",    .timeout,
    if (.quiet) "--no-progress"
  )

  secs_ <- .ner_system("docker", as.character(args_), "lexnlp")
  if (!fs::file_exists(out_)) cli::cli_abort("lexnlp reported success but wrote no parquet.")
  structure(as.character(out_), elapsed = secs_)
}

#' Extract with matcon-extract, through its console entry point
#'
#' RETURNS SEVERAL PATHS, unlike the other two. Entities map to modules inside the package -- DATE
#' and TERM both come from dateregex, MONEY from moneyregex -- so one call can run several
#' extractors, and each writes its own parquet.
#'
#' THE CALLER NEVER NAMES A MODEL. Which version of dateregex runs is whatever is installed, and the
#' extractor stamps it.
#'
#' @inheritParams .ner_run_lexnlp
#' @param .describe Output of ner_describe(), used to predict which files should appear.
#' @return Character vector of paths, with an "elapsed" attribute.
.ner_run_matcon <- function(.path_in, .out_dir, .entity, .id_col, .text_col, .max_chars,
                            .workers, .batch_size, .timeout, .describe, .quiet) {
  if (FALSE) {
    .path_in    <- .lP$Output$Sample
    .out_dir    <- .lP$Output$Stage
    .entity     <- c("GPE", "DATE", "TERM", "MONEY", "REDACT")
    .id_col     <- "DocID"
    .text_col   <- "TextRaw"
    .max_chars  <- 0L
    .workers    <- 5L
    .batch_size <- 64L
    .timeout    <- 120L
    .describe   <- ner_describe()
    .quiet      <- FALSE
  }

  fs::dir_create(.out_dir)

  args_ <- c(
    "-m", "matcon_extract",
    .path_in,
    "--out-dir",    .out_dir,
    "--label",      .entity,
    "--id-col",     .id_col,
    "--text-col",   .text_col,
    "--max-chars",  .max_chars,
    "--n-process",  .workers,
    "--chunk-size", .batch_size,
    "--timeout",    .timeout,
    if (.quiet) "--no-progress"
  )

  secs_ <- .ner_system(.ner_python("matcon"), as.character(args_), "matcon")

  # The package writes <engine>__<model>.parquet per model, so the paths are discovered rather than
  # predicted: R does not know which models the requested entities resolved to until it asks.
  want_ <- .describe |>
    dplyr::filter(.data$Family == "matcon", .data$Entity %in% .entity) |>
    dplyr::distinct(.data$Model)
  out_  <- fs::path(.out_dir, paste0("matcon__", want_$Model, ".parquet"))
  miss_ <- out_[!fs::file_exists(out_)]
  if (length(miss_) > 0L) {
    cli::cli_abort(
      "matcon reported success but {length(miss_)} expected file{?s} {?is/are} absent: \\
       {paste(fs::path_file(miss_), collapse = ', ')}"
    )
  }

  structure(as.character(out_), elapsed = secs_)
}

#' Extract with one spaCy model
#'
#' One model per call. Vectorising over models was considered and rejected: extract_spacy.py takes
#' --model singular, so four models are four subprocess launches whether the loop sits in R or in
#' Python, and the benchmark needs a wall clock per model rather than one for the set.
#'
#' @inheritParams .ner_run_lexnlp
#' @param .model spaCy model name.
#' @param .device NULL resolves per model -- see .ner_spacy_device().
#' @return Path written, with an "elapsed" attribute.
.ner_run_spacy <- function(.path_in, .out_dir, .model, .entity, .id_col, .text_col, .max_chars,
                           .workers, .batch_size, .timeout, .device, .quiet) {
  if (FALSE) {
    .path_in    <- .lP$Output$Sample
    .out_dir    <- .lP$Output$Stage
    .model      <- "en_core_web_trf"
    .entity     <- c("ORG", "PERSON", "GPE")
    .id_col     <- "DocID"
    .text_col   <- "TextRaw"
    .max_chars  <- 0L
    .workers    <- 5L
    .batch_size <- 32L
    .timeout    <- 600L
    .device     <- NULL
    .quiet      <- FALSE
  }

  fs::dir_create(.out_dir)
  dev_ <- if (is.null(.device)) .ner_spacy_device(.model = .model) else .device
  out_ <- fs::path(.out_dir, paste0("spacy__", .model, ".parquet"))

  args_ <- c(
    fs::path(.ner_family_dir("spacy"), "extract_spacy.py"),
    .path_in,
    "--output",     out_,
    "--model",      .model,
    "--label",      .entity,
    "--id-col",     .id_col,
    "--text-col",   .text_col,
    "--max-chars",  .max_chars,
    "--device",     dev_,
    "--n-process",  .workers,
    "--batch-size", .batch_size,
    "--timeout",    .timeout,
    if (.quiet) "--no-progress"
  )

  secs_ <- .ner_system(.ner_python("spacy"), as.character(args_), paste0("spacy:", .model))
  if (!fs::file_exists(out_)) cli::cli_abort("spaCy reported success but wrote no parquet.")
  structure(as.character(out_), elapsed = secs_)
}

#' Run one family over a staged parquet
#'
#' THE ONE ENTRY POINT. Every extraction in the project goes through it -- the benchmark, the sample
#' pass in 04A and the corpus pass in 04C -- so the corpus is extracted by the code the sample was
#' extracted with, which is what makes the two stores comparable rather than merely similar.
#'
#' It writes files and returns their paths. Nothing is ingested here: see ner_ingest().
#'
#' SECONDS IS PER CALL, NOT PER FILE. matcon returns several parquets from one invocation and they
#' share one wall clock; splitting it between them would invent a number.
#'
#' @param .family One of "lexnlp", "matcon", "spacy".
#' @param .model spaCy model name. NULL for lexnlp and matcon, where the family versions itself.
#' @param .entity Character vector of entities to request. Required; validated before anything runs.
#' @param .path_in Staged parquet carrying .id_col and .text_col.
#' @param .out_dir Directory the family writes into. A temporary one for a benchmark.
#' @param .describe Output of ner_describe(), for validation and for the model tags.
#' @param .id_col,.text_col Column names in the staged parquet.
#' @param .max_chars Truncate each document to its first N characters. 0 disables.
#' @param .workers Worker processes. Ignored by spaCy on a GPU, which forces one.
#' @param .batch_size Documents per unit of work.
#' @param .timeout Per-document cap in seconds.
#' @param .device spaCy device override. NULL resolves per model.
#' @param .image Docker image name for the LexNLP family.
#' @param .quiet Suppress the extractor's own progress bar.
#' @return Tibble, one row per parquet written: Family, Model, SpecHash, Entities (list), Path,
#'   Seconds.
ner_extract <- function(.family,
                        .model      = NULL,
                        .entity,
                        .path_in,
                        .out_dir,
                        .describe,
                        .id_col     = "DocID",
                        .text_col   = "TextRaw",
                        .max_chars  = 0L,
                        .workers    = .ner_workers,
                        .batch_size = .ner_batch_size,
                        .timeout    = .ner_timeout,
                        .device     = NULL,
                        .image      = "contracts-lexnlp",
                        .quiet      = FALSE) {
  if (FALSE) {
    .family     <- "matcon"
    .model      <- NULL
    .entity     <- c("GPE", "DATE", "TERM", "MONEY", "REDACT")
    .path_in    <- .lP$Output$Sample
    .out_dir    <- .lP$Output$Stage
    .describe   <- tab_describe
    .id_col     <- "DocID"
    .text_col   <- "TextRaw"
    .max_chars  <- 0L
    .workers    <- 5L
    .batch_size <- 64L
    .timeout    <- 120L
    .device     <- NULL
    .image      <- "contracts-lexnlp"
    .quiet      <- FALSE
  }

  if (missing(.entity) || length(.entity) == 0L) {
    cli::cli_abort("{.arg .entity} is required: name what this family is being asked to produce.")
  }
  .family <- match.arg(.family, .ner_families)
  ner_check_entities(.describe = .describe, .family = .family, .entity = .entity)

  if (.family == "spacy") {
    if (is.null(.model)) cli::cli_abort("spaCy needs {.arg .model}: it is the only family with one.")
    ok_ <- .describe |>
      dplyr::filter(.data$Family == "spacy", .data$Model == .model, .data$Ready)
    if (nrow(ok_) == 0L) cli::cli_abort("spaCy model {(.model)} is not installed.")
  } else if (!is.null(.model)) {
    cli::cli_abort("{(.family)} versions itself; leave {.arg .model} NULL.")
  }

  if (!fs::file_exists(.path_in)) cli::cli_abort("No staged text at {.path {(.path_in)}}.")
  fs::dir_create(.out_dir)

  paths_ <- switch(
    .family,
    lexnlp = .ner_run_lexnlp(
      .path_in = .path_in, .out_dir = .out_dir, .entity = .entity, .id_col = .id_col,
      .text_col = .text_col, .max_chars = .max_chars, .workers = .workers,
      .batch_size = .batch_size, .timeout = .timeout, .image = .image, .quiet = .quiet
    ),
    matcon = .ner_run_matcon(
      .path_in = .path_in, .out_dir = .out_dir, .entity = .entity, .id_col = .id_col,
      .text_col = .text_col, .max_chars = .max_chars, .workers = .workers,
      .batch_size = .batch_size, .timeout = .timeout, .describe = .describe, .quiet = .quiet
    ),
    spacy = .ner_run_spacy(
      .path_in = .path_in, .out_dir = .out_dir, .model = .model, .entity = .entity,
      .id_col = .id_col, .text_col = .text_col, .max_chars = .max_chars, .workers = .workers,
      .batch_size = .batch_size, .timeout = .timeout, .device = .device, .quiet = .quiet
    )
  )
  secs_ <- attr(paths_, "elapsed")

  # THE MODEL TAG IS READ BACK, NEVER TYPED. Each file is named <engine>__<model>.parquet by the
  # extractor that wrote it, so the tag is recovered from the filename and reconciled against the
  # description -- which is where its spec hash comes from.
  tibble::tibble(Path = as.character(paths_)) |>
    dplyr::mutate(
      Family = .family,
      Model  = stringi::stri_replace_first_regex(fs::path_ext_remove(fs::path_file(.data$Path)),
                                                 "^[^_]+__", "")
    ) |>
    dplyr::left_join(
      .describe |>
        dplyr::filter(.data$Family == .family) |>
        dplyr::summarise(SpecHash = dplyr::first(.data$SpecHash), .by = c("Family", "Model")),
      by = dplyr::join_by(Family, Model)
    ) |>
    dplyr::mutate(
      Entities = purrr::map(.data$Model, \(.m) {
        can_ <- .describe$Entity[.describe$Family == .family & .describe$Model == .m]
        intersect(.entity, can_)
      }),
      Seconds = secs_
    ) |>
    dplyr::select("Family", "Model", "SpecHash", "Entities", "Path", "Seconds")
}


# 3. The store: one database per family ---------------------------------------------------------------------------------
#
# Three files, three schemas, no negotiation. Each holds one table per entity plus three fixed
# tables: `runs` (the ledger), `manifest` (what produced the rows) and `bench` (timings).
#
# THE LEDGER IS SEPARATE FROM THE SPANS, and that is the point of it. A document processed for GPE
# that matched nothing has no row in the gpe table, so without a ledger it is indistinguishable from
# one that was never processed -- and the orchestrator would re-extract it forever while believing
# it complete.
#
# SENTINELS ARE NOT STORED AS SPANS. The extractors emit a null-offset row for a document that was
# processed and matched nothing; under the old flat store those rows lived in the label tables and
# every query had to filter them out. Here the ledger records the outcome and the entity tables hold
# only real spans, which is one fewer thing every downstream WHERE clause has to remember.

#' Where a family's database lives
#'
#' @param .dir Output directory, e.g. 2_output/04A-EntityExtract/Output.
#' @param .family Family name.
#' @return Path to the family's DuckDB file.
ner_db_path <- function(.dir, .family) {
  if (FALSE) {
    .dir    <- .lP$Output$Store
    .family <- "matcon"
  }
  fs::path(.dir, paste0(.family, ".duckdb"))
}

#' Open a family's database
#'
#' @param .db_path Path to the DuckDB file.
#' @param .read_only Open read-only. A report must not be able to modify what it reports on.
#' @return A DBI connection.
ner_db_connect <- function(.db_path, .read_only = FALSE) {
  if (FALSE) {
    .db_path   <- ner_db_path(.dir = .lP$Output$Store, .family = "matcon")
    .read_only <- TRUE
  }
  fs::dir_create(fs::path_dir(.db_path))
  con_ <- DBI::dbConnect(duckdb::duckdb(dbdir = as.character(.db_path), read_only = .read_only))

  # THREADS FROM AN OPTION, NOT AN ARGUMENT. Every caller of this function would otherwise have to
  # carry a dial that concerns exactly one of them. 04D's daemons set mc.duckdb_threads; nowhere else
  # does, so nowhere else changes. A dozen workers each opening a connection that helps itself to
  # every core is 288 threads on a 24-core machine, and the contention costs more than the read.
  thr_ <- getOption("mc.duckdb_threads", NULL)
  if (!is.null(thr_)) DBI::dbExecute(con_, paste0("SET threads TO ", as.integer(thr_)))

  con_
}

#' Create the three fixed tables
#'
#' Idempotent: every statement is CREATE ... IF NOT EXISTS. The ENTITY tables are deliberately absent
#' here -- they are created on first ingest from the staged parquet's own schema, because a
#' hand-written column list is a statement about a file it never reads.
#'
#' @param .con Connection from ner_db_connect().
#' @return .con, invisibly.
ner_db_init <- function(.con) {
  if (FALSE) {
    .con <- ner_db_connect(.db_path = tempfile(fileext = ".duckdb"))
  }

  # FOUR STATES, NOT TWO. An extractor that crashed on a document must stay distinguishable from one
  # that found nothing, or a pattern failing on a whole class of documents looks exactly like that
  # class having no matches.
  #
  # Model is part of the key, which has a useful consequence for matcon: bump gazetteer to v3 and
  # there is no row for (DocID, gazetteer-v3, GPE), so GPE re-extracts and DATE, MONEY and REDACT do
  # not. The invalidation is exactly as wide as the change.
  DBI::dbExecute(.con, "
    CREATE TABLE IF NOT EXISTS runs (
      DocID   VARCHAR   NOT NULL,
      Model   VARCHAR   NOT NULL,
      Entity  VARCHAR   NOT NULL,
      Status  VARCHAR   NOT NULL,   -- hit | nohit | timeout | error
      RunAt   TIMESTAMP NOT NULL,
      PRIMARY KEY (DocID, Model, Entity)
    )")

  # WHAT PRODUCED THE ROWS THAT ARE IN THIS FILE. The ledger catches a DECLARED version bump; this
  # catches an UNDECLARED change -- a pattern edited without a bump, an image rebuilt, a spaCy model
  # upgraded in place -- which is the failure no key can see.
  DBI::dbExecute(.con, "
    CREATE TABLE IF NOT EXISTS manifest (
      Model     VARCHAR   NOT NULL,
      Entity    VARCHAR   NOT NULL,
      SpecHash  VARCHAR,
      WrittenAt TIMESTAMP NOT NULL,
      PRIMARY KEY (Model, Entity)
    )")

  # DrawHash rather than NDoc alone. The previous key trusted a document COUNT, so a different draw
  # of the same size would have served a stale timing -- a cache keyed on a number rather than on
  # its contents.
  DBI::dbExecute(.con, "
    CREATE TABLE IF NOT EXISTS bench (
      Model    VARCHAR   NOT NULL,
      Entities VARCHAR   NOT NULL,
      Batch    INTEGER   NOT NULL,
      Workers  INTEGER   NOT NULL,
      NDoc     INTEGER   NOT NULL,
      DrawHash VARCHAR   NOT NULL,
      Machine  VARCHAR   NOT NULL,
      Seconds  DOUBLE    NOT NULL,
      DocPerS  DOUBLE    NOT NULL,
      RunAt    TIMESTAMP NOT NULL,
      PRIMARY KEY (Model, Entities, Batch, Workers, NDoc, DrawHash, Machine)
    )")

  invisible(.con)
}

#' Tables that are bookkeeping rather than spans
#'
#' NAMED IN ONE PLACE, because every caller that asks "which entity tables are there" derives the
#' answer by subtraction. 04C adds `corpus` and `failures` to a family database, and a list written
#' out at each call site would have counted those as entities -- putting a corpus index through the
#' offset check and reporting it as an entity with no spans.
.ner_meta_tables <- c("runs", "manifest", "bench", "corpus", "failures")

#' The entity tables this database currently holds
#'
#' Views are excluded rather than assumed absent: DBI::dbListTables() returns them alongside tables.
#'
#' @param .con Connection.
#' @return Character vector of entity table names, lower case.
ner_db_tables <- function(.con) {
  if (FALSE) {
    .con <- ner_db_connect(.db_path = tempfile(fileext = ".duckdb"))
  }
  setdiff(DBI::dbListTables(.con), .ner_meta_tables)
}

#' Record what produced the rows, and refuse a silent change of meaning
#'
#' A MODEL TAG UNCHANGED WITH THE HASH MOVED means somebody edited a rule without bumping the
#' version, and the rows already in the store came from a different rule than the one installed now.
#' That is an abort rather than a warning: the same function serves the corpus pass, where
#' re-extracting is days, and the choice belongs to whoever made the edit.
#'
#' NA IS A CHANGE TOO, and the first version of this missed it by requiring both sides to be
#' non-missing. The case is live: the LexNLP image predates image_spec.py and carries no spec_hash
#' label, so it stamps NA. Rebuilding it later would move NA to a real hash -- a genuine change of
#' provenance, since the rows in the store came from an image whose contents nothing recorded -- and
#' the comparison would have waved it through. Missing and present are different findings.
#'
#' @param .con Connection.
#' @param .model Model tag.
#' @param .entity Entities this model wrote.
#' @param .spec_hash Fingerprint reported by the family now. NA where it cannot supply one.
#' @return Rows written, invisibly.
ner_manifest_write <- function(.con, .model, .entity, .spec_hash) {
  if (FALSE) {
    .con       <- con_matcon
    .model     <- "gazetteer-v2"
    .entity    <- "GPE"
    .spec_hash <- "a3f91c7e2b40"
  }

  have_ <- DBI::dbGetQuery(.con, glue::glue(
    "SELECT * FROM manifest WHERE Model = '{.model}'"
  )) |>
    tibble::as_tibble()

  if (nrow(have_) > 0L) {
    same_  <- (is.na(have_$SpecHash) & is.na(.spec_hash)) |
      (!is.na(have_$SpecHash) & !is.na(.spec_hash) & have_$SpecHash == .spec_hash)
    moved_ <- have_[!same_, , drop = FALSE]

    if (nrow(moved_) > 0L) {
      was_ <- unique(moved_$SpecHash)
      was_ <- ifelse(is.na(was_), "(none recorded)", was_)
      now_ <- if (is.na(.spec_hash)) "(none reported)" else .spec_hash
      cli::cli_abort(c(
        "{(.model)} now reports spec hash {(now_)}; the store holds rows written under \\
         {paste(was_, collapse = ', ')}.",
        "x" = "The tag has not moved but the provenance has, so the stored rows mean something else.",
        "i" = "Bump the model version, or clear this model with ner_db_clear() and re-extract."
      ))
    }
  }

  # A MISSING HASH IS NOT A NEUTRAL DEFAULT. It means this family cannot say what produced these
  # rows, so the undeclared-change check can never fire for them. Said once per write rather than
  # left as a silent NA in a column nobody reads.
  if (is.na(.spec_hash)) {
    cli::cli_alert_warning(
      "{(.model)} reports no spec hash, so provenance is not recorded for \\
       {length(.entity)} entit{?y/ies} and a later change to it cannot be detected."
    )
  }

  rows_ <- tibble::tibble(
    Model = .model, Entity = .entity, SpecHash = .spec_hash, WrittenAt = Sys.time()
  )
  DBI::dbExecute(.con, glue::glue(
    "DELETE FROM manifest WHERE Model = '{.model}'
       AND Entity IN ({paste0(\"'\", .entity, \"'\", collapse = ', ')})"
  ))
  DBI::dbAppendTable(.con, "manifest", as.data.frame(rows_))
  invisible(nrow(rows_))
}

#' Record an outcome for every (document, entity) a run covered
#'
#' FOUR STATES. A document appears once per requested entity with exactly one of:
#'
#'   hit      at least one span for that entity
#'   nohit    processed, found nothing -- NOT the same as never processed
#'   timeout  cut off by the per-document cap
#'   error    the extractor raised
#'
#' The last two are read from the sentinel row's LabelRaw, which the Python side stamps as
#' "timeout:<name>" or "error:<name>". Before the error state existed, a crash produced a bare
#' sentinel and was recorded as nohit, so a pattern failing on a class of documents was
#' indistinguishable from that class having no matches.
#'
#' @param .con Connection.
#' @param .path Staged parquet.
#' @param .model Model tag.
#' @param .entity Entities requested.
#' @return Rows written, invisibly.
.ner_ledger_write <- function(.con, .path, .model, .entity) {
  if (FALSE) {
    .con    <- con_matcon
    .path   <- fs::path(.lP$Output$Stage, "matcon__dateregex-v3.parquet")
    .model  <- "dateregex-v3"
    .entity <- c("DATE", "TERM")
  }

  tab_ <- arrow::read_parquet(.path, col_select = c("DocID", "Start", "Label", "LabelRaw")) |>
    tibble::as_tibble()

  docs_ <- unique(tab_$DocID)

  fail_ <- tab_ |>
    dplyr::filter(is.na(.data$Start), !is.na(.data$LabelRaw)) |>
    dplyr::mutate(Fail = dplyr::if_else(
      stringi::stri_startswith_fixed(.data$LabelRaw, "timeout:"), "timeout", "error"
    )) |>
    dplyr::select("DocID", "Fail") |>
    dplyr::distinct()

  hit_ <- tab_ |>
    dplyr::filter(!is.na(.data$Start)) |>
    dplyr::distinct(.data$DocID, .data$Label) |>
    dplyr::mutate(Hit = TRUE)

  led_ <- tidyr::expand_grid(DocID = docs_, Label = .entity) |>
    dplyr::left_join(hit_, by = dplyr::join_by(DocID, Label)) |>
    dplyr::left_join(fail_, by = dplyr::join_by(DocID)) |>
    dplyr::transmute(
      .data$DocID,
      Model  = .model,
      Entity = .data$Label,
      Status = dplyr::case_when(
        !is.na(.data$Fail)         ~ .data$Fail,
        !is.na(.data$Hit)          ~ "hit",
        .default                   = "nohit"
      ),
      RunAt = Sys.time()
    )

  # SCOPED TO THE DOCUMENTS IN THIS FILE, not to the model. 04A ingests one file covering the whole
  # sample; 04C ingests one per chunk. Deleting by model alone would be correct in the first case and
  # would erase the whole ledger on every chunk in the second.
  DBI::dbWriteTable(.con, "tmp_docs", data.frame(DocID = docs_), temporary = TRUE, overwrite = TRUE)
  DBI::dbExecute(.con, glue::glue(
    "DELETE FROM runs WHERE Model = '{.model}'
       AND Entity IN ({paste0(\"'\", .entity, \"'\", collapse = ', ')})
       AND DocID IN (SELECT DocID FROM tmp_docs)"
  ))
  DBI::dbAppendTable(.con, "runs", as.data.frame(led_))
  DBI::dbRemoveTable(.con, "tmp_docs")

  invisible(nrow(led_))
}

#' Move one staged parquet's spans into this family's entity tables
#'
#' THE TABLE IS CREATED FROM THE ENTITY'S DECLARED SCHEMA, on first sight, and appended to
#' thereafter. It used to be created from the PARQUET's schema, which is a different and wider
#' thing: a module writes one file for every label it owns, so lexnlp's four labels give a
#' 24-column file in which a DATE row fills five columns and leaves thirteen null -- and every one
#' of those thirteen became a column of lexnlp.date. A reader then had to know which columns
#' belonged to which label before they could read anything.
#'
#' ent_stored_cols() IS THE DECLARATION, and it is derived from .ent_extras and .ent_rename rather
#' than being a second copy of them. The table therefore holds exactly what ent_load_entity() will
#' select from it, which is the property that makes the schema its own documentation.
#'
#' SOURCE ORDER. This calls into _Entity.R, which is sourced after this file everywhere in the
#' project. R resolves at call time so the order is satisfied, but the dependency is real and is
#' named here rather than left to be discovered: _NER.R owns the store, _Entity.R owns what each
#' entity means, and the schema is where the two meet.
#'
#' Three columns do not survive: `Engine`, because the file is the family; `Label`, because the table
#' is the entity; and `Model`, except for spaCy, where models are real and the column separates them.
#'
#' Everything runs in DuckDB against read_parquet(), so nothing large passes through R and no R type
#' is imposed on a column the extractor typed.
#'
#' @param .con Connection.
#' @param .path Staged parquet.
#' @param .family Family name; decides whether Model is kept.
#' @param .model Model tag.
#' @param .entity Entities requested. Rows outside this set are refused rather than stored: the
#'   ledger records what was asked for, so a row for an unrequested entity has no ledger entry and is
#'   invisible to every completeness check downstream.
#' @return Tibble: Entity, NSpan, NDoc.
.ner_spans_write <- function(.con, .path, .family, .model, .entity) {
  if (FALSE) {
    .con    <- con_matcon
    .path   <- fs::path(.lP$Output$Stage, "matcon__dateregex-v3.parquet")
    .family <- "matcon"
    .model  <- "dateregex-v3"
    .entity <- c("DATE", "TERM")
  }

  path_ <- as.character(fs::path_real(.path))
  cols_ <- arrow::open_dataset(sources = path_)$schema$names

  seen_ <- DBI::dbGetQuery(.con, glue::glue(
    "SELECT DISTINCT Label FROM read_parquet('{path_}') WHERE Label IS NOT NULL"
  ))$Label
  extra_ <- setdiff(seen_, .entity)
  if (length(extra_) > 0L) {
    cli::cli_abort(c(
      "{fs::path_file(.path)} carries {length(extra_)} entit{?y/ies} that {?was/were} not \\
       requested: {paste(extra_, collapse = ', ')}.",
      "i" = "The ledger records what was requested, so unrequested rows would never count as done."
    ))
  }

  # NO exists() GUARD HERE, AND THERE WAS ONE. It aborted with "_Entity.R must be sourced after
  # _NER.R", which is a plausible cause and was not the actual one: the file had simply not been
  # copied over. R's own error names the missing function exactly and guesses nothing, and a
  # diagnostic that names the wrong cause is worse than none -- the same lesson the preflight probe
  # recorded when it reported a missing corpus store that was present.
  out_ <- tibble::tibble(Entity = character(0), NSpan = integer(0), NDoc = integer(0))

  for (ent_ in .entity) {
    tbl_   <- tolower(ent_)
    where_ <- glue::glue("Label = '{ent_}' AND Start IS NOT NULL")

    # PER ENTITY, NOT PER FILE. This is the whole change: the selection is computed inside the loop
    # from what this entity declares, where it used to be computed once from the parquet and reused
    # for every label the file carried.
    want_ <- ent_stored_cols(.family = .family, .entity = ent_)
    gone_ <- setdiff(want_, cols_)
    if (length(gone_) > 0L) {
      cli::cli_abort(c(
        "{fs::path_file(.path)} carries no {paste(gone_, collapse = ', ')} for {(ent_)}.",
        "i" = "Declared by ent_stored_cols(); present in the file: \\
               {paste(cols_, collapse = ', ')}."
      ))
    }
    sel_ <- paste0("\"", want_, "\"", collapse = ", ")

    # WHERE FALSE gives the schema and no rows, so a first ingest creates the table and a later one
    # is a no-op.
    DBI::dbExecute(.con, glue::glue(
      "CREATE TABLE IF NOT EXISTS {tbl_} AS
         SELECT {sel_} FROM read_parquet('{path_}') WHERE FALSE"
    ))

    # A TABLE THAT ALREADY EXISTS UNDER A DIFFERENT SCHEMA IS NOT SILENTLY APPENDED TO. Narrowing
    # the declaration leaves every store built under the old one holding columns this ingest will
    # not fill, and an INSERT would either fail on arity or, worse, succeed against a table whose
    # extra columns are quietly null forever. Same shape as the manifest guard: a store half in one
    # schema and half in another is a file no downstream check would catch.
    hold_ <- DBI::dbListFields(.con, tbl_)
    if (!identical(hold_, want_)) {
      cli::cli_abort(c(
        "{(tbl_)} exists with a different schema than {(.family)}/{(ent_)} now declares.",
        "*" = "holds:    {paste(hold_, collapse = ', ')}",
        "*" = "declares: {paste(want_, collapse = ', ')}",
        "i" = "Delete this family's database and re-ingest; the staged parquets are unchanged, so
               nothing is re-extracted."
      ))
    }

    scope_ <- if (.family == "spacy") glue::glue(" AND Model = '{.model}'") else ""
    DBI::dbExecute(.con, glue::glue(
      "DELETE FROM {tbl_}
        WHERE DocID IN (SELECT DISTINCT DocID FROM read_parquet('{path_}')){scope_}"
    ))
    n_ <- DBI::dbExecute(.con, glue::glue(
      "INSERT INTO {tbl_} SELECT {sel_} FROM read_parquet('{path_}') WHERE {where_}"
    ))

    nd_ <- DBI::dbGetQuery(.con, glue::glue(
      "SELECT COUNT(DISTINCT DocID) AS n FROM read_parquet('{path_}') WHERE {where_}"
    ))$n[[1L]]

    out_ <- dplyr::bind_rows(out_, tibble::tibble(
      Entity = ent_, NSpan = as.integer(n_), NDoc = as.integer(nd_)
    ))
  }

  out_
}

# 3a. The schema of an entity table --------------------------------------------------------------------------------
#
# MOVED HERE FROM _Entity.R, AND THE REASON IS 04A AND 04C. Both source this file and never
# _Entity.R -- they extract and ingest, and never turn a span into a variable -- so once
# .ner_spans_write() began declaring each table's schema, it was calling into a file two of the
# eight documents in the family do not load. It worked interactively, where both had been sourced by
# hand, and would have failed on a clean render.
#
# THE DECLARATIONS ARE SCHEMA, AND SCHEMA BELONGS WITH THE STORE. That is the principled version of
# the same point: this file owns the tables, so it owns what a table holds. _Entity.R reads from
# here and nothing here reads from _Entity.R, which is the one-way dependency the two files had
# before and briefly lost.

#' The columns a family calls something else
#'
#' RENAMED ON READ, AND ONLY ON READ. The stores keep each family's own column names, which is what
#' makes a column traceable to the documentation of the engine that produced it. But two of LexNLP's
#' names cannot survive contact with R:
#'
#'   TypeAbbr holds the string "NA" for a National Association -- every national bank in the corpus
#'   -- and R prints that identically to a missing value. A column whose most common value is
#'   indistinguishable from absence in every table it appears in is a defect waiting for a reader.
#'
#'   Name means the resolved company for ORG and the resolved place for GPE. One column called Name
#'   meaning two different things is the kind of thing that is obvious while writing and invisible
#'   six weeks later.
#'
#' So the rename happens here, in one function, next to the schema it serves -- rather than in the
#' ingest, where it would have made the store disagree with LexNLP's own documentation.
.ent_rename <- list(
  lexnlp = c(
    NameCore       = "Name",
    LegalForm      = "TypeAbbr",     # LexNLP's company_type_abbr: CORP, INC, LLC -- and NA
    LegalFormFull  = "TypeFull",
    LegalFormLabel = "TypeLabel",
    GeoName        = "NameEn",
    GeoAlias       = "Alias",
    GeoCategory    = "EntityCategory",
    # ENTITY IS LEXNLP'S WORD FOR A GEOGRAPHIC ENTITY, and unqualified it reads as an entity in this
    # project's sense -- an ORG, a DATE, a span. Both columns are 0 of 2,522 on ORG rows and 2,177
    # of 2,177 on GPE, so the prefix says what the count says.
    GeoEntityId       = "EntityId",
    GeoEntityPriority = "EntityPriority",
    DateScore      = "Score"
  ),
  matcon = c(
    # Iso2 is a SUBDIVISION code here and never a country; Iso3 is a country code and carries USA on
    # every US entity, so neither name means what the identical name means in the LexNLP store.
    SubIso      = "Iso2",
    CountryIso3 = "Iso3"
  )
)

#' What each family emits for one entity, under this project's names
#'
#' DECLARED IN ONE PLACE BECAUSE THE FAMILIES NO LONGER SHARE A SCHEMA. Under the flat store every
#' GPE row carried the union of both producers' columns and a caller named one extras list for both.
#' One database per family ended that: matcon's gpe table has GeoKey, IsWord, NParent and MatchKind
#' and no GeoName; LexNLP's has GeoName, GeoAlias and GeoCategory and none of the others. A single
#' list would have asked each family for the other's columns.
#'
#' 04A's numbers say why the two are worth keeping apart rather than reconciling here: they agree on
#' only 44.5% of GPE mentions while agreeing on boundaries 97.6% of the time where they meet, which
#' is complements at different tiers rather than rivals at one.
.ent_extras <- list(
  lexnlp = list(
    # THE LEGAL-FORM TAXONOMY IS FREE, AND IT WAS BEING DISCARDED. LegalFormFull and
    # LegalFormLabel move exactly with LegalForm -- 2,053 of 2,522 organisations carry all three or
    # none -- so the coarser classification, whether a counterparty is a Corporation or a
    # Partnership, is available wherever the abbreviation is and costs no coverage at all. It was
    # emitted by the extractor and dropped at the seam, which is how a column somebody wants in six
    # weeks disappears without anyone deciding it should.
    ORG   = c("NameCore", "LegalForm", "LegalFormFull", "LegalFormLabel", "Description"),
    # GeoEntityId and GeoEntityPriority are GEOGRAPHIC and belong nowhere else: 2,177 of 2,177 on
    # GPE rows and 0 of 2,522 on ORG. Priority is what LexNLP ranks by where several entities could
    # match one string, so it is the evidence for a resolution this project did not make.
    GPE   = c("GeoName", "GeoAlias", "GeoCategory", "Iso2", "Iso3",
              "GeoEntityId", "GeoEntityPriority"),
    DATE  = c("DateValue", "DateScore"),
    MONEY = c("Amount", "Currency")
    #
    # NameAbbr IS DELIBERATELY ABSENT, AND THAT IS A MEASUREMENT RATHER THAN AN OVERSIGHT. LexNLP
    # declares it and fills it on 6 of 2,522 ORG spans -- 0.2%. Storing it would put back a column
    # null on 99.8% of the rows of the only entity it could belong to, which is exactly the shape
    # this declaration exists to remove. The dictionary records the count so the exclusion can be
    # revisited on evidence rather than rediscovered.
  ),
  matcon = list(
    # THE TWO ISO COLUMNS ARE RENAMED because matcon and LexNLP use the same two names for four
    # different quantities. matcon's own docstring is explicit: Iso2 resolves "all 50 states, 81% of
    # populated places, 95% of counties, and NEVER a country", while Iso3 is "every country, and USA
    # for every US entity". LexNLP's Iso2 is a country code OR a subdivision code and its Iso3 is a
    # country code on countries alone.
    #
    # Mapping both families onto Iso2/Iso3 was a defect that rendered clean: geo_country() reads Iso3
    # first, matcon's Iso3 happens to be a country code, and the answer came out right by luck while
    # the ISO-3166-2 prefix recovery sat dead at 0.0% of matcon's rows. A shared column NAME is not a
    # shared QUANTITY, and the only place that can be settled is the read.
    GPE    = c("GeoKey", "IsWord", "NParent", "SubIso", "CountryIso3", "MatchKind"),
    DATE   = c("DateValue"),
    TERM   = c("TermN", "TermUnit", "TermYears"),
    MONEY  = c("Amount", "Currency"),
    REDACT = character(),
    # LAW CARRIES NONE, AND THAT IS THE DESIGN. lawregex locates a governing-law clause and says
    # which cue opened it; the jurisdiction is the GPE span sitting INSIDE that clause, resolved
    # once by the gazetteer rather than twice. Two extractors resolving Delaware would leave two
    # answers to reconcile.
    #
    # DECLARED EXPLICITLY BECAUSE ent_extras() ABORTS ON AN UNKNOWN ENTITY, which is right: an
    # entity that silently returned no extras would produce a table with the right rows and the
    # wrong columns, and nothing downstream would say so. Every new label is registered here.
    LAW    = character()
  ),
  spacy = list(
    ORG    = character(),
    PERSON = character(),
    GPE    = character()
  )
)

#: The context columns every matcon extractor emits, APPENDED rather than declared per entity.
#:
#: THE SAME ARRANGEMENT AS _io.Emitter, WHICH IS THE POINT. Python puts these two on the emitter
#: rather than in each module's EXTRAS so that no module can forget them and the column order is
#: uniform: CORE, the module's own extras, CueBefore, CueAfter. Declaring them per entity here would
#: reintroduce exactly the drift that arrangement removes -- five entries that must agree, and
#: nothing to notice when one does not.
.ent_cues <- c("CueBefore", "CueAfter")

#: Which families emit them. All three, and the third needed H5 fixed first.
#:
#: THE BLOCK WAS AN IDENTITY PROBLEM, NOT A SCHEDULING ONE. ner_describe() used to set the spaCy
#: family's SpecHash from the MODEL version -- 3.8.0 -- so editing extract_spacy.py would have added
#: two columns while the identity stayed exactly where it was. ner_manifest_write() would have seen
#: nothing changed and admitted the new rows beside the old, leaving the store holding two
#: generations under one tag with nothing able to tell them apart.
#:
#: ner_spacy_spec() now hashes the script AND the model together, so all three families report the
#: same KIND of thing and an edit to any of them moves it. The version is still reported, in Note,
#: where it is metadata rather than identity.
.ent_cue_families <- c("matcon", "lexnlp", "spacy")

#' The extras one family emits for one entity
#'
#' @param .family Family name.
#' @param .entity Entity name.
#' @return Character vector, possibly empty.
ent_extras <- function(.family, .entity) {
  if (FALSE) {
    .family <- "matcon"
    .entity <- "GPE"
  }
  fam_ <- .ent_extras[[.family]]
  if (is.null(fam_)) cli::cli_abort("No extras declared for family {(.family)}.")
  out_ <- fam_[[toupper(.entity)]]
  if (is.null(out_)) {
    cli::cli_abort(c(
      "{(.family)} declares no {(.entity)} extras.",
      "i" = "It declares: {paste(names(fam_), collapse = ', ')}."
    ))
  }
  # THE CUE COLUMNS ARE APPENDED, NOT DECLARED. Every matcon entity carries them and none of the
  # entries above names them, so an entity added tomorrow gets them without anyone remembering to.
  # Order matters and matches the emitter: the module's own extras first, then CueBefore, CueAfter.
  if (.family %in% .ent_cue_families) out_ <- c(out_, .ent_cues)
  out_
}

#' The columns one entity's table holds in the store
#'
#' THE STORE'S SCHEMA, DECLARED ONCE AND ENFORCED AT BOTH ENDS. A module writes ONE parquet for
#' every label it owns, so the file carries the union of those labels' extras -- lexnlp's four
#' labels give a 24-column file in which a DATE row fills five of them and leaves thirteen null.
#' The ingest used to take that file's schema wholesale, so lexnlp.date held Name, TypeAbbr and
#' Iso2 and a reader had to know which columns belonged to which label before they could read
#' anything.
#'
#' THE PARQUET IS TRANSPORT; THE TABLE IS SCHEMA. They do not have to match. A wide staging file
#' costs nothing -- parquet stores an all-null column as metadata -- and narrowing at the ingest
#' means the table holds exactly what ent_load_entity() will select from it. "What is used where"
#' stops being a question a reader has to answer, because the schema IS the answer.
#'
#' NO NEW DECLARATION. .ent_extras already says what each family emits for each entity and
#' .ent_rename already maps this project's names onto the store's, so the stored column list is
#' derivable from what is here rather than being a second copy that can drift from it. This
#' function is what ent_load_entity() computed inline; lifting it out is what lets the WRITE use
#' the same expression as the READ.
#'
#' MODEL IS IN THE LIST FOR SPACY AND ONLY SPACY, and it is in the TABLE even though
#' ent_load_entity() does not return it: spaCy is the one family where several models compete for
#' one entity, so the column is what a read filters on.
#'
#' @param .family Family name.
#' @param .entity Entity name.
#' @return Character vector of stored column names, in table order.
ent_stored_cols <- function(.family, .entity) {
  if (FALSE) {
    .family <- "lexnlp"
    .entity <- "DATE"
  }

  ren_  <- .ent_rename[[.family]]
  want_ <- ent_extras(.family, .entity)
  stor_ <- if (is.null(ren_)) want_ else dplyr::coalesce(unname(ren_[want_]), want_)

  c("DocID", "Start", "Stop", "Span", "LabelRaw",
    if (identical(.family, "spacy")) "Model" else NULL,
    stor_)
}


#' Ingest everything one extraction produced
#'
#' Spans, ledger and manifest together, in that order, per staged file. Splitting them across calls
#' would allow a store that reports work done and holds none of it.
#'
#' @param .con Connection to THIS family's database.
#' @param .staged Tibble returned by ner_extract().
#' @param .entity Entities that were requested.
#' @return Tibble: Model, Entity, NSpan, NDoc.
ner_ingest <- function(.con, .staged, .entity) {
  if (FALSE) {
    .con    <- con_matcon
    .staged <- staged_matcon
    .entity <- c("GPE", "DATE", "TERM", "MONEY", "REDACT")
  }

  if (nrow(.staged) == 0L) cli::cli_abort("Nothing staged to ingest.")
  fam_ <- unique(.staged$Family)
  if (length(fam_) != 1L) {
    cli::cli_abort("A staged set must come from one family; this one names {length(fam_)}.")
  }

  purrr::pmap(
    list(.staged$Path, .staged$Model, .staged$SpecHash, .staged$Entities),
    \(.p, .m, .h, .e) {
      want_ <- intersect(.entity, .e)
      if (length(want_) == 0L) return(tibble::tibble())

      spans_ <- .ner_spans_write(
        .con = .con, .path = .p, .family = fam_, .model = .m, .entity = want_
      )
      .ner_ledger_write(.con = .con, .path = .p, .model = .m, .entity = want_)
      ner_manifest_write(.con = .con, .model = .m, .entity = want_, .spec_hash = .h)

      dplyr::mutate(spans_, Model = .m, .before = 1L)
    }
  ) |>
    purrr::list_rbind()
}

#' Which documents a (model, entity) has not seen
#'
#' The question every pass asks before it stages anything. A ledger row is what counts as seen,
#' whatever its status: a timeout was tried, and re-trying it on every render would spend the cap
#' again on the same pathological document.
#'
#' @param .con Connection.
#' @param .doc_ids Candidate documents.
#' @param .model Model tag.
#' @param .entity One entity.
#' @return Character vector of DocIDs with no ledger entry.
ner_db_missing <- function(.con, .doc_ids, .model, .entity) {
  if (FALSE) {
    .con     <- con_matcon
    .doc_ids <- tab_sample$DocID
    .model   <- "dateregex-v3"
    .entity  <- "DATE"
  }
  done_ <- DBI::dbGetQuery(.con, glue::glue(
    "SELECT DISTINCT DocID FROM runs WHERE Model = '{.model}' AND Entity = '{.entity}'"
  ))$DocID
  setdiff(.doc_ids, done_)
}

#' Remove one model entirely
#'
#' Spans, ledger and manifest together. Removing one without the others leaves a store that reports
#' work done and holds none of it, or holds rows nothing knows about.
#'
#' @param .con Connection.
#' @param .model Model tag.
#' @param .entity Entities to clear. NULL clears every entity this model wrote.
#' @return Rows removed per table, invisibly.
ner_db_clear <- function(.con, .model, .entity = NULL) {
  if (FALSE) {
    .con    <- con_matcon
    .model  <- "gazetteer-v2"
    .entity <- NULL
  }

  ent_ <- if (is.null(.entity)) {
    DBI::dbGetQuery(.con, glue::glue(
      "SELECT DISTINCT Entity FROM runs WHERE Model = '{.model}'"
    ))$Entity
  } else {
    .entity
  }
  if (length(ent_) == 0L) {
    cli::cli_alert_info("Nothing recorded for {(.model)}; nothing to clear.")
    return(invisible(tibble::tibble()))
  }

  in_ <- paste0("'", ent_, "'", collapse = ", ")
  have_ <- ner_db_tables(.con = .con)

  out_ <- purrr::map(ent_, \(.e) {
    tbl_ <- tolower(.e)
    n_   <- if (tbl_ %in% have_) {
      DBI::dbExecute(.con, glue::glue("DELETE FROM {tbl_} WHERE TRUE"))
    } else {
      0L
    }
    tibble::tibble(Entity = .e, NSpan = as.integer(n_))
  }) |>
    purrr::list_rbind()

  DBI::dbExecute(.con, glue::glue(
    "DELETE FROM runs WHERE Model = '{.model}' AND Entity IN ({in_})"
  ))
  DBI::dbExecute(.con, glue::glue(
    "DELETE FROM manifest WHERE Model = '{.model}' AND Entity IN ({in_})"
  ))

  cli::cli_alert_success(
    "Cleared {(.model)}: {format(sum(out_$NSpan), big.mark = ',')} \\
     {cli::qty(sum(out_$NSpan))}span{?s} and their ledger rows."
  )
  invisible(out_)
}

#' What this family's database holds
#'
#' @param .con Connection.
#' @return Tibble: Model, Entity, NSpan, nHit, nNoHit, nTimeout, nError, SpecHash.
ner_db_summary <- function(.con) {
  if (FALSE) {
    .con <- ner_db_connect(.db_path = ner_db_path(.lP$Output$Store, "matcon"), .read_only = TRUE)
  }

  led_ <- DBI::dbGetQuery(.con, "
    SELECT Model, Entity,
           SUM(CASE WHEN Status = 'hit'     THEN 1 ELSE 0 END) AS nHit,
           SUM(CASE WHEN Status = 'nohit'   THEN 1 ELSE 0 END) AS nNoHit,
           SUM(CASE WHEN Status = 'timeout' THEN 1 ELSE 0 END) AS nTimeout,
           SUM(CASE WHEN Status = 'error'   THEN 1 ELSE 0 END) AS nError
      FROM runs GROUP BY Model, Entity") |>
    tibble::as_tibble()

  if (nrow(led_) == 0L) return(tibble::tibble())

  have_  <- ner_db_tables(.con = .con)
  spans_ <- led_ |>
    dplyr::mutate(NSpan = purrr::map_int(.data$Entity, \(.e) {
      tbl_ <- tolower(.e)
      if (!tbl_ %in% have_) return(0L)
      as.integer(DBI::dbGetQuery(.con, glue::glue("SELECT COUNT(*) AS n FROM {tbl_}"))$n[[1L]])
    }))

  man_ <- DBI::dbGetQuery(.con, "SELECT Model, Entity, SpecHash FROM manifest") |>
    tibble::as_tibble()

  spans_ |>
    dplyr::left_join(man_, by = dplyr::join_by(Model, Entity)) |>
    dplyr::mutate(dplyr::across(c("nHit", "nNoHit", "nTimeout", "nError"), as.integer)) |>
    dplyr::select("Model", "Entity", "NSpan", "nHit", "nNoHit", "nTimeout", "nError", "SpecHash") |>
    dplyr::arrange(.data$Model, .data$Entity)
}


# 3b. Where the families meet ------------------------------------------------------------------------------------------
#
# THE ONLY PLACE THE THREE DATABASES ARE OPEN AT ONCE, and it is deliberately a separate connection
# rather than one of the family connections with the others hung off it. A comparison should not
# depend on which family's file happened to be open when it ran, and a read-only attach cannot write
# to a store the section is describing.
#
# DuckDB reads across attached databases without copying, so this costs nothing beyond the open.

#' Open an in-memory connection with every family database attached read-only
#'
#' @param .dir Output directory holding the family databases.
#' @param .families Family names to attach. Absent files are skipped and named.
#' @return A DBI connection; the caller disconnects it.
ner_attach <- function(.dir, .families = .ner_families) {
  if (FALSE) {
    .dir      <- .lP$Output$Store
    .families <- .ner_families
  }

  con_ <- DBI::dbConnect(duckdb::duckdb())
  got_ <- character(0)

  for (f_ in .families) {
    p_ <- ner_db_path(.dir = .dir, .family = f_)
    if (!fs::file_exists(p_)) next
    DBI::dbExecute(con_, glue::glue("ATTACH '{as.character(p_)}' AS {f_} (READ_ONLY)"))
    got_ <- c(got_, f_)
  }

  miss_ <- setdiff(.families, got_)
  if (length(miss_) > 0L) {
    cli::cli_alert_warning(
      "No database for {paste(miss_, collapse = ', ')}, so {?it is/they are} absent from every \\
       comparison below."
    )
  }
  attr(con_, "families") <- got_
  con_
}

#' Which (family, entity) tables the attached databases actually hold
#'
#' READ FROM THE CATALOGUE, NOT FROM A LIST IN R. Which entity tables exist is a property of the
#' files, and a table that was never created because its extractor never ran is exactly the case a
#' hand-written list would get wrong.
#'
#' @param .con Connection from ner_attach().
#' @return Tibble: Family, Entity, Table (fully qualified), NSpan.
ner_attached_tables <- function(.con) {
  if (FALSE) {
    .con <- ner_attach(.dir = .lP$Output$Store)
  }

  tab_ <- DBI::dbGetQuery(.con, "
    SELECT database_name AS Family, table_name AS Tbl
      FROM duckdb_tables()
     WHERE table_name NOT IN ('runs', 'manifest', 'bench', 'corpus', 'failures')") |>
    tibble::as_tibble()

  if (nrow(tab_) == 0L) return(tibble::tibble())

  tab_ |>
    dplyr::mutate(
      Entity = toupper(.data$Tbl),
      Table  = paste0(.data$Family, ".", .data$Tbl),
      NSpan  = purrr::map_int(.data$Table, \(.t) as.integer(
        DBI::dbGetQuery(.con, glue::glue("SELECT COUNT(*) AS n FROM {.t}"))$n[[1L]]
      ))
    ) |>
    dplyr::select("Family", "Entity", "Table", "NSpan") |>
    dplyr::arrange(.data$Entity, .data$Family)
}


# 4. Benchmark ---------------------------------------------------------------------------------------------------------
#
# A LEDGER, NOT A SWITCH. Every render reports the whole grid and measures only the cells not already
# stored, so the first render pays the full cost and later ones cost seconds. Nothing is skippable
# and nothing is switched off -- which is the difference between this and an eval: false, and the
# reason a document can carry an expensive measurement without becoming unrenderable.
#
# THE MACHINE IS PART OF THE KEY because a timing from a different box is a different measurement.
# Without it, running this on a laptop would silently overwrite the Mac Studio numbers. Re-measuring
# means deleting rows, which is an action rather than a setting.
#
# ONE DRAW, FROZEN, FOR EVERY CELL. Runtime is dominated by document length, so cells that process
# different documents cannot be compared: a difference in throughput would confound scheduling with
# the draw. Every cell in a family runs the same documents and only the batch size varies.

#' This machine, as a benchmark key
#'
#' @return Short string identifying the host and its core count.
ner_machine <- function() {
  if (FALSE) {
    # no arguments
  }
  paste0(Sys.info()[["sysname"]], "-", Sys.info()[["machine"]], "-", parallel::detectCores(), "c")
}

#' Freeze a draw and stage it
#'
#' Written once and reused by every cell. Returns the path and a fingerprint of the DocID set, which
#' is what the bench key stores instead of a bare count.
#'
#' @param .tab_sample The sample, carrying DocID and TextRaw.
#' @param .path Where to stage the draw.
#' @param .n Documents to draw.
#' @param .seed Fixed, so the draw is the same on every machine and every render.
#' @return List: Path, DocIDs, DrawHash, NDoc.
ner_bench_draw <- function(.tab_sample, .path, .n = 320L, .seed = 42L) {
  if (FALSE) {
    .tab_sample <- tab_sample
    .path       <- fs::path(tempdir(), "bench-draw.parquet")
    .n          <- 320L
    .seed       <- 42L
  }

  if (nrow(.tab_sample) < .n) {
    cli::cli_abort("The sample holds {nrow(.tab_sample)} documents; the draw asks for {(.n)}.")
  }

  set.seed(.seed)
  draw_ <- .tab_sample |>
    dplyr::arrange(.data$DocID) |>
    dplyr::slice_sample(n = .n) |>
    dplyr::select("DocID", "TextRaw")

  fs::dir_create(fs::path_dir(.path))
  arrow::write_parquet(draw_, .path)

  list(
    Path     = .path,
    DocIDs   = draw_$DocID,
    DrawHash = substr(rlang::hash(sort(draw_$DocID)), 1L, 12L),
    NDoc     = nrow(draw_)
  )
}

#' Measure one benchmark cell, or read it from the store
#'
#' Runs the family, times it, discards the output. Extraction and ingest are separate functions
#' precisely so this can exist: a benchmark that wrote to the store would pollute what it measures.
#'
#' @param .con Connection to the family's database.
#' @param .family Family name.
#' @param .model spaCy model name, or NULL.
#' @param .entity Entities to request.
#' @param .draw Output of ner_bench_draw(). A prefix is taken where .n_doc is smaller.
#' @param .n_doc Documents to use from the draw.
#' @param .batch Batch size to test.
#' @param .workers Worker count.
#' @param .describe Output of ner_describe().
#' @param .timeout Per-document cap.
#' @param .machine Machine key.
#' @return One-row tibble, invisibly.
ner_bench_cell <- function(.con, .family, .model = NULL, .entity, .draw, .n_doc,
                           .batch, .workers, .describe, .timeout = .ner_timeout,
                           .machine = ner_machine()) {
  if (FALSE) {
    .con      <- con_matcon
    .family   <- "matcon"
    .model    <- NULL
    .entity   <- c("GPE", "DATE", "TERM", "MONEY", "REDACT")
    .draw     <- draw
    .n_doc    <- 320L
    .batch    <- 32L
    .workers  <- 5L
    .describe <- tab_describe
    .timeout  <- 600L
    .machine  <- ner_machine()
  }

  key_ <- if (is.null(.model)) .family else .model
  ent_ <- paste(sort(.entity), collapse = "+")

  hit_ <- DBI::dbGetQuery(.con, glue::glue(
    "SELECT * FROM bench
      WHERE Model = '{key_}' AND Entities = '{ent_}' AND Batch = {.batch}
        AND Workers = {.workers} AND NDoc = {.n_doc} AND DrawHash = '{.draw$DrawHash}'
        AND Machine = '{.machine}'"
  ))
  if (nrow(hit_) == 1L) return(invisible(tibble::as_tibble(hit_)))

  dir_ <- fs::path(tempdir(), paste0("bench-", key_, "-", .batch))
  fs::dir_create(dir_)
  on.exit(if (fs::dir_exists(dir_)) fs::dir_delete(dir_), add = TRUE)

  # A PREFIX OF THE SAME DRAW, not a second draw. The transformer runs at one worker on the GPU, so
  # it needs fewer documents to be measurable in a sensible time -- but they must be the same
  # documents, or its numbers describe a different length distribution from everything else.
  path_ <- if (.n_doc == .draw$NDoc) {
    .draw$Path
  } else {
    sub_ <- fs::path(dir_, "draw-prefix.parquet")
    arrow::read_parquet(.draw$Path) |>
      dplyr::slice_head(n = .n_doc) |>
      arrow::write_parquet(sub_)
    sub_
  }

  staged_ <- ner_extract(
    .family     = .family,
    .model      = .model,
    .entity     = .entity,
    .path_in    = path_,
    .out_dir    = dir_,
    .describe   = .describe,
    .workers    = .workers,
    .batch_size = .batch,
    .timeout    = .timeout,
    .quiet      = TRUE
  )
  secs_ <- staged_$Seconds[[1L]]

  row_ <- tibble::tibble(
    Model    = key_,
    Entities = ent_,
    Batch    = as.integer(.batch),
    Workers  = as.integer(.workers),
    NDoc     = as.integer(.n_doc),
    DrawHash = .draw$DrawHash,
    Machine  = .machine,
    Seconds  = secs_,
    DocPerS  = .n_doc / secs_,
    RunAt    = Sys.time()
  )
  DBI::dbAppendTable(.con, "bench", as.data.frame(row_))
  invisible(row_)
}

#' Sweep batch sizes for one family, measuring only what is missing
#'
#' @param .con Connection to the family's database.
#' @param .family Family name.
#' @param .model spaCy model name, or NULL.
#' @param .entity Entities to request -- the set the pass will actually ask for, so the timing
#'   describes the work that will be done rather than some other work.
#' @param .draw Output of ner_bench_draw().
#' @param .n_doc Documents to use from the draw.
#' @param .batches Batch sizes to sweep.
#' @param .workers Worker count, fixed.
#' @param .describe Output of ner_describe().
#' @param .timeout Per-document cap.
#' @return This family's whole grid for this machine, invisibly.
ner_bench_run <- function(.con, .family, .model = NULL, .entity, .draw, .n_doc,
                          .batches = c(4L, 8L, 16L, 32L, 64L), .workers = .ner_workers,
                          .describe, .timeout = .ner_timeout) {
  if (FALSE) {
    .con      <- con_matcon
    .family   <- "matcon"
    .model    <- NULL
    .entity   <- c("GPE", "DATE", "TERM", "MONEY", "REDACT")
    .draw     <- draw
    .n_doc    <- 320L
    .batches  <- c(4L, 8L, 16L, 32L, 64L)
    .workers  <- 5L
    .describe <- tab_describe
    .timeout  <- 600L
  }

  key_ <- if (is.null(.model)) .family else .model

  # SKIP, DO NOT ABORT. Two of three families being measurable is a partial result worth having, and
  # the ledger means filling in the third later costs only the third. Aborting the render would
  # discard the two that worked to report the one that did not.
  #
  # THE VERDICT WAS TAKEN EARLIER, and saying so is the difference between a two-minute fix and a
  # diagnosis. ner_describe() runs once near the top and every chunk reads that object, which is
  # right for a render -- the table and the behaviour come from the same moment -- and a trap in an
  # interactive session, where starting a service afterwards changes the environment and not the
  # object.
  if (!ner_ready(.describe = .describe, .family = .family, .model = .model)) {
    cli::cli_alert_warning(
      "{(key_)} is not available, so nothing was measured: \\
       {(ner_why_not(.describe = .describe, .family = .family, .model = .model))}."
    )
    cli::cli_alert_info(
      "That verdict comes from ner_describe(), which ran earlier. If the environment has changed \\
       since, re-run the describe chunk before this one."
    )
    return(invisible(tibble::as_tibble(DBI::dbGetQuery(.con, glue::glue(
      "SELECT * FROM bench WHERE Machine = '{ner_machine()}' ORDER BY Model, Batch"
    )))))
  }

  cli::cli_alert_info(
    "{(key_)}: {length(.batches)} batch size{?s} over {format(.n_doc, big.mark = ',')} \\
     {cli::qty(.n_doc)}document{?s} at {(.workers)} worker{?s}. Stored cells are not re-run."
  )

  purrr::walk(.batches, \(.b) ner_bench_cell(
    .con      = .con,
    .family   = .family,
    .model    = .model,
    .entity   = .entity,
    .draw     = .draw,
    .n_doc    = .n_doc,
    .batch    = .b,
    .workers  = .workers,
    .describe = .describe,
    .timeout  = .timeout
  ))

  out_ <- DBI::dbGetQuery(.con, glue::glue(
    "SELECT * FROM bench WHERE Machine = '{ner_machine()}' ORDER BY Model, Batch"
  ))
  invisible(tibble::as_tibble(out_))
}

#' Report a benchmark grid and name the fastest cell per model
#'
#' Names it; does not adopt it. A run whose parameters depend on a timing produces different work on
#' different machines and nothing records which, so the declared value stays declared and this table
#' is the evidence for whatever gets typed into the configuration.
#'
#' @param .tab Rows returned by ner_bench_run().
#' @param .live Model tags that can run right now, typically from a plan. Where given, models
#'   measured here but absent from it are named, because a stored timing survives the removal of the
#'   thing that produced it.
#' @return .tab, invisibly.
ner_report_bench <- function(.tab, .live = character()) {
  if (FALSE) {
    .tab  <- tab_bench
    .live <- c("matcon", "en_core_web_trf")
  }

  if (nrow(.tab) == 0L) {
    cli::cli_alert_warning("No benchmark rows for this machine.")
    return(invisible(.tab))
  }

  show_ <- .tab |>
    dplyr::transmute(
      .data$Model, .data$Batch, .data$Workers, .data$NDoc,
      Seconds = round(.data$Seconds, 1),
      DocPerS = round(.data$DocPerS, 2)
    ) |>
    dplyr::arrange(.data$Model, .data$Batch)

  tbl_say(.tab = show_, .title = "Throughput by model and batch size")

  # STORED TIMINGS OUTLIVE THE THING THAT PRODUCED THEM, and that is the cache working rather than a
  # fault -- but a reader meeting "lexnlp 4.08 doc/s" here and "lexnlp unavailable" three sections
  # up needs the two reconciled, or one of them looks wrong.
  gone_ <- setdiff(unique(.tab$Model), .live)
  if (length(.live) > 0L && length(gone_) > 0L) {
    cli::cli_alert_info(
      "{paste(gone_, collapse = ', ')} {?is/are} measured here and not available now. A benchmark \\
       row is a record of a measurement on this machine; it is not a claim that the model can run \\
       today."
    )
  }

  best_ <- .tab |>
    dplyr::slice_max(.data$DocPerS, n = 1L, by = "Model") |>
    dplyr::transmute(.data$Model, BestBatch = .data$Batch, DocPerS = round(.data$DocPerS, 2))

  tbl_say(.tab = best_, .title = "Fastest cell per model")
  cli::cli_alert_info(
    "Reported, not adopted. The batch size the pass uses is declared in the configuration; \\
     this table is the evidence for what gets typed there."
  )

  invisible(.tab)
}
