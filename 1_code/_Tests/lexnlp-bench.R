# LexNLP throughput: where the corpus time actually goes ----
#
# WHY THIS EXISTS
# LexNLP is the expensive engine in the corpus plan and the only one whose cost is not obviously
# linear in text length: it runs a maxent NER, a date grammar, a money grammar and, since GPE was
# enabled, a dictionary scan of 437 entities over the whole document. Every rate this project has
# was read off a progress bar on 2,500 hand-picked labelled contracts, which is neither a random
# draw nor the corpus's length distribution.
#
# WHAT IT MEASURES, IN ORDER OF HOW MUCH THE ANSWER IS WORTH
#   A. The marginal cost of GPE. If the geoentity pass is most of the bill, dropping it from the
#      corpus and keeping the gazetteer is a decision worth hours; if it is a tenth, it is free.
#   B. Whether n_process = 24 is right. Docker Desktop allocates its own CPU budget, so the host's
#      core count is not the number that matters and more workers past the allocation is contention
#      rather than throughput.
#   C. Whether chunk_size = 8 is right. Small chunks balance uneven document lengths and cost one
#      inter-process round trip each; large chunks amortise the round trip and let one long
#      document strand a worker.
#   D. How the rate varies with document length, which is what makes a sample rate extrapolate to a
#      corpus or not.
#
# STARTUP IS MEASURED AND SUBTRACTED. Every invocation pays for container start, LexNLP import,
# NLTK load and, for GPE, building the locator over 437 entities. On a 300-document benchmark that
# fixed cost is a visible share of the total; in the real pass it is amortised over a 2,000-document
# chunk. Reporting the gross rate would understate throughput and would understate it MOST for the
# arm that builds the locator, which is exactly the arm under test.
#
# WHAT PRINTS WHEN. Block A is three one-document runs and lands in under a minute. Block B is the
# headline -- three runs at one setting -- and answers the geoentity question in about ninety
# seconds; if that answer settles the decision, stop there. Block C is the tuning sweep, which is
# the long part: it prints one line per cell as each finishes and accumulates into .bench_partial,
# so an interrupt keeps everything already measured. Block E is four short runs at the end.
#
# Reads only. Writes nothing but its own scratch parquet in tempdir().
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new columns; pure ASCII.


# 1. Configuration ----

source("1_code/_Commons/_Initialize.R", encoding = "UTF-8")
source("1_code/_Commons/_Utils.R", encoding = "UTF-8")
source("1_code/_Commons/_NER.R", encoding = "UTF-8")

.path_text <- here::here("2_output", "04A-EntityExtract", "sample_text.parquet")
.dir_bench <- fs::dir_create(fs::path(tempdir(), "lexbench"))
.corpus_n  <- 1460000L   # documents in the full Exhibit-10 tree, for the projection
.seed      <- 42L

# A random draw, not the first N. Document order in the sample is filing order, and filing order
# correlates with length: the first twenty documents ran at 5.9 doc/s in an earlier smoke test and
# were not representative of anything.
.n_docs <- 300L

# The grids. Deliberately small -- this is a search for the shape of the surface, not its optimum,
# and every extra cell is a container start.
.grid_labels <- list(
  "ORG,DATE,MONEY" = c("ORG", "DATE", "MONEY"),
  "GPE only"       = "GPE",
  "all four"       = c("ORG", "GPE", "DATE", "MONEY")
)
.grid_nproc <- c(8L, 16L, 24L, 32L)
.grid_chunk <- c(8L, 32L, 128L)

stopifnot(fs::file_exists(.path_text))


# 2. The sample ----

tab_all <- arrow::read_parquet(.path_text) |>
  dplyr::mutate(Chars = stringi::stri_length(.data$TextRaw))

tab_bench <- withr::with_seed(.seed, dplyr::slice_sample(tab_all, n = .n_docs))
arrow::write_parquet(
  dplyr::select(tab_bench, DocID, TextRaw),
  fs::path(.dir_bench, "bench.parquet")
)

# One document, for the startup measurement. Chosen as the shortest in the draw so that its own
# extraction contributes as little as possible to what is being attributed to startup.
arrow::write_parquet(
  tab_bench |> dplyr::slice_min(.data$Chars, n = 1L) |> dplyr::select(DocID, TextRaw),
  fs::path(.dir_bench, "one.parquet")
)

cli::cli_h2("Benchmark sample")
tibble::tibble(
  Docs      = nrow(tab_bench),
  MedChars  = as.integer(stats::median(tab_bench$Chars)),
  P90Chars  = as.integer(stats::quantile(tab_bench$Chars, 0.9)),
  MaxChars  = as.integer(max(tab_bench$Chars)),
  TotalMB   = round(sum(tab_bench$Chars) / 1024^2, 1)
) |>
  print(width = Inf)

cli::cli_alert_info(
  "Compare MedChars against the corpus. The sample is hand-picked material contracts; the corpus \\
   carries every Exhibit 10 filed, including short amendments. A rate measured here transfers only \\
   as far as the length distributions agree."
)


# 3. One timed run ----

bench_run <- function(.labels, .nproc, .chunk, .input, .tag) {
  if (FALSE) {
    .labels <- c("ORG", "DATE", "MONEY")
    .nproc  <- 24L
    .chunk  <- 8L
    .input  <- fs::path(.dir_bench, "bench.parquet")
    .tag    <- "probe"
  }

  out_ <- fs::path(.dir_bench, paste0("out_", .tag, ".parquet"))
  if (fs::file_exists(out_)) fs::file_delete(out_)

  t0_ <- Sys.time()
  ok_ <- tryCatch({
    ner_lexnlp(
      .inputs      = .input,
      .output      = out_,
      .labels      = .labels,
      .timeout     = 240L,
      .chunk_size  = .chunk,
      .n_process   = .nproc,
      .no_progress = TRUE,   # the bar would interleave across runs and tell us nothing here
      .quiet       = TRUE
    )
    TRUE
  }, error = function(.e) {
    cli::cli_alert_danger("failed: {conditionMessage(.e)}")
    FALSE
  })
  secs_ <- as.numeric(difftime(Sys.time(), t0_, units = "secs"))

  n_cand_ <- if (ok_ && fs::file_exists(out_)) {
    arrow::read_parquet(out_) |> dplyr::filter(!is.na(.data$Start)) |> nrow()
  } else {
    NA_integer_
  }
  if (fs::file_exists(out_)) fs::file_delete(out_)

  tibble::tibble(Seconds = secs_, Candidates = as.integer(n_cand_), Ok = ok_)
}


# 4. Startup ----
# One document per label set. Whatever this costs is fixed per invocation and is subtracted from
# every timing below, because the corpus pass pays it once per 2,000-document chunk rather than
# once per 300.

cli::cli_h2("A. Fixed startup cost, by label set")
tab_start <- purrr::imap(.grid_labels, function(.lab, .nm) {
  r_ <- bench_run(.labels = .lab, .nproc = 8L, .chunk = 8L,
                  .input = fs::path(.dir_bench, "one.parquet"), .tag = "start")
  tibble::tibble(LabelSet = .nm, StartSeconds = round(r_$Seconds, 1))
}) |>
  purrr::list_rbind()

print(tab_start, width = Inf)
cli::cli_alert_info(
  "Container start, LexNLP import and NLTK load are common to every row; the difference between \\
   the GPE rows and the others is the locator build over 437 entities. If that difference is large \\
   it argues for BIGGER chunks in the corpus pass, since the cost is paid once per chunk."
)


# 5. The headline, before the sweep ----
# THE QUESTION WORTH ANSWERING FIRST is what the geoentity pass costs, because it decides whether
# LexNLP GPE goes on the corpus at all. Three runs at one reasonable setting answer it in about
# ninety seconds. The tuning sweep below is worth a further twenty minutes only if the answer here
# says the engine is staying.

.head_nproc <- 24L
.head_chunk <- 32L

cli::cli_h2("B. What the geoentity pass costs")

tab_head <- purrr::imap(.grid_labels, function(.lab, .nm) {
  cli::cli_alert_info("running {(.nm)} at n_process={(.head_nproc)}, chunk={(.head_chunk)} ...")
  r_ <- bench_run(.labels = .lab, .nproc = .head_nproc, .chunk = .head_chunk,
                  .input = fs::path(.dir_bench, "bench.parquet"), .tag = "head")
  net_ <- pmax(r_$Seconds - tab_start$StartSeconds[match(.nm, tab_start$LabelSet)], 0.1)
  out_ <- tibble::tibble(
    LabelSet   = .nm,
    NetSeconds = round(net_, 1),
    DocsPerSec = round(.n_docs / net_, 2),
    CorpusHrs  = round(.corpus_n / (.n_docs / net_) / 3600, 1),
    Candidates = r_$Candidates
  )
  print(out_, width = Inf)
  out_
}) |>
  purrr::list_rbind()

if (nrow(tab_head) == 3L) {
  h3_  <- tab_head$CorpusHrs[tab_head$LabelSet == "ORG,DATE,MONEY"]
  hall_ <- tab_head$CorpusHrs[tab_head$LabelSet == "all four"]
  cli::cli_alert_warning(
    "Three labels: {h3_}h. All four: {hall_}h. GPE adds {round(hall_ - h3_, 1)}h, which is \\
     {round(100 * (hall_ - h3_) / hall_)}% of the LexNLP bill. THAT IS THE DECISION."
  )
}


# 6. The tuning sweep ----
# Runs cell by cell and prints each as it completes, because a twenty-minute block that returns
# nothing until it finishes cannot be stopped on the strength of what it has already shown. Results
# accumulate in a list that survives an interrupt, so a partial sweep is still a sweep.

cli::cli_h2("C. Throughput by label set, worker count and chunk size")

tab_grid <- tidyr::expand_grid(
  LabelSet = names(.grid_labels),
  NProc    = .grid_nproc,
  Chunk    = .grid_chunk
)

# The headline runs give a rate per label set; the sweep costs roughly the same work per cell,
# adjusted for how far each worker count sits from the one just measured.
.eta_min <- tab_grid |>
  dplyr::left_join(dplyr::select(tab_head, LabelSet, NetSeconds), by = dplyr::join_by(LabelSet)) |>
  dplyr::mutate(Est = .data$NetSeconds * (.head_nproc / .data$NProc) +
                  tab_start$StartSeconds[1]) |>
  dplyr::pull(.data$Est) |>
  sum() / 60

cli::cli_alert_info(
  "{nrow(tab_grid)} cell{?s}, roughly {round(.eta_min)} minute{?s}. Each prints as it finishes; \\
   stop with Escape and .bench_partial holds what completed."
)

.bench_partial <- list()

for (i_ in seq_len(nrow(tab_grid))) {
  row_ <- tab_grid[i_, ]
  cat(sprintf("[%2d/%2d] %-16s nproc=%-3d chunk=%-4d ... ",
              i_, nrow(tab_grid), row_$LabelSet, row_$NProc, row_$Chunk))
  utils::flush.console()

  r_ <- bench_run(.labels = .grid_labels[[row_$LabelSet]], .nproc = row_$NProc,
                  .chunk = row_$Chunk, .input = fs::path(.dir_bench, "bench.parquet"),
                  .tag = paste0(row_$NProc, "_", row_$Chunk))
  net_ <- pmax(r_$Seconds - tab_start$StartSeconds[match(row_$LabelSet, tab_start$LabelSet)], 0.1)
  dps_ <- .n_docs / net_

  cat(sprintf("%6.1fs net  %6.2f doc/s  %5.1f corpus-hours\n", net_, dps_,
              .corpus_n / dps_ / 3600))
  utils::flush.console()

  .bench_partial[[i_]] <- tibble::tibble(
    LabelSet   = row_$LabelSet,
    NProc      = row_$NProc,
    Chunk      = row_$Chunk,
    Seconds    = round(r_$Seconds, 1),
    NetSeconds = round(net_, 1),
    Candidates = r_$Candidates,
    Ok         = r_$Ok
  )
}

tab_bench_out <- purrr::list_rbind(.bench_partial) |>
  dplyr::mutate(
    DocsPerSec = round(.n_docs / .data$NetSeconds, 2),
    CorpusHrs  = round(.corpus_n / .data$DocsPerSec / 3600, 1)
  )

tab_bench_out |>
  dplyr::select(LabelSet, NProc, Chunk, NetSeconds, DocsPerSec, CorpusHrs, Candidates) |>
  print(n = Inf, width = Inf)


# 6b. Which of the four labels costs the time ----
# THE BLOCK THAT DECIDES WHETHER THE 83 HOURS ARE NEGOTIABLE. Section B showed that GPE is 6% of
# the LexNLP bill and the other three are the rest -- but not which of the three. That distinction
# is worth measuring rather than assuming, because two of them have a cheap alternative already in
# the corpus plan: dateregex for DATE and moneyregex-v4 for MONEY, both of them regex engines
# running at a fraction of this cost.
#
# If the date grammar is the expensive one, dropping DATE from the LexNLP policy costs nothing that
# is not already covered and may take days off the pass. If it is the maxent company NER, the 83
# hours are the price of the organisation variable and there is nothing to negotiate -- no other
# engine on this corpus can find parties.

cli::cli_h2("C2. Cost per label, run alone")

tab_solo <- purrr::map(c("ORG", "DATE", "MONEY", "GPE"), function(.l) {
  cat(sprintf("  %-6s ... ", .l)); utils::flush.console()
  r_ <- bench_run(.labels = .l, .nproc = .head_nproc, .chunk = 8L,
                  .input = fs::path(.dir_bench, "bench.parquet"), .tag = paste0("solo_", .l))
  net_ <- pmax(r_$Seconds - stats::median(tab_start$StartSeconds), 0.1)
  hrs_ <- .corpus_n / (.n_docs / net_) / 3600
  cat(sprintf("%6.1fs net  %6.2f doc/s  %6.1f corpus-hours  %6d candidates\n",
              net_, .n_docs / net_, hrs_, r_$Candidates)); utils::flush.console()
  tibble::tibble(Label = .l, NetSeconds = round(net_, 1),
                 DocsPerSec = round(.n_docs / net_, 2), CorpusHrs = round(hrs_, 1),
                 Candidates = r_$Candidates)
}) |>
  purrr::list_rbind() |>
  dplyr::arrange(dplyr::desc(.data$CorpusHrs))

print(tab_solo, width = Inf)

cli::cli_alert_info(
  "These do not sum to the all-four figure and should not: container start, the parquet read and \\
   the tokenisation are paid once per invocation whatever is asked for. Read the ORDER and the \\
   RATIOS, not the total. The label at the top of this table is where the corpus time goes."
)



# 7. Reading it ----

cli::cli_h2("D. Best cell per label set")
tab_bench_out |>
  dplyr::filter(.data$Ok) |>
  dplyr::slice_max(.data$DocsPerSec, n = 1L, by = LabelSet) |>
  dplyr::select(LabelSet, NProc, Chunk, DocsPerSec, CorpusHrs) |>
  print(width = Inf)

cli::cli_alert_warning(
  "DOCKER'S CPU ALLOCATION IS THE CEILING, not the host's core count. If DocsPerSec stops rising \\
   between two NProc values the allocation has been reached, and higher settings buy contention. \\
   Check Docker Desktop's resource settings against where the curve flattens."
)


# 8. Does the rate hold across document lengths ----
# The question that decides whether any of the above extrapolates. Four draws, one per length
# quartile of the sample, at the best setting found.

cli::cli_h2("E. Rate by document length")

.best <- tab_bench_out |>
  dplyr::filter(.data$Ok, .data$LabelSet == "all four") |>
  dplyr::slice_max(.data$DocsPerSec, n = 1L)

tab_quart <- tab_all |>
  dplyr::mutate(Quartile = dplyr::ntile(.data$Chars, 4L)) |>
  dplyr::slice_sample(n = 60L, by = Quartile)

purrr::map(sort(unique(tab_quart$Quartile)), function(.q) {
  sub_ <- dplyr::filter(tab_quart, .data$Quartile == .q)
  arrow::write_parquet(dplyr::select(sub_, DocID, TextRaw), fs::path(.dir_bench, "q.parquet"))
  r_ <- bench_run(.labels = c("ORG", "GPE", "DATE", "MONEY"),
                  .nproc = .best$NProc, .chunk = .best$Chunk,
                  .input = fs::path(.dir_bench, "q.parquet"), .tag = paste0("q", .q))
  net_ <- pmax(r_$Seconds - tab_start$StartSeconds[tab_start$LabelSet == "all four"], 0.1)
  tibble::tibble(
    Quartile   = .q,
    Docs       = nrow(sub_),
    MedChars   = as.integer(stats::median(sub_$Chars)),
    NetSeconds = round(net_, 1),
    DocsPerSec = round(nrow(sub_) / net_, 2),
    CharsPerSec = as.integer(sum(sub_$Chars) / net_)
  )
}) |>
  purrr::list_rbind() |>
  print(n = Inf, width = Inf)

cli::cli_alert_info(
  "CharsPerSec is the column that transfers. If it is flat across quartiles the cost is linear in \\
   text and the corpus projection is a division; if it falls in the longest quartile something is \\
   superlinear -- the maxent chunker and the greedy overlap resolution are both candidates -- and \\
   the corpus will be slower than the sample suggests, because the corpus has the longer tail."
)

fs::dir_delete(.dir_bench)
