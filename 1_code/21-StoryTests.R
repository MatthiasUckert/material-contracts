# 21-StoryTests.R --------------------------------------------------------------------------------------
#
# The library behind 21-StoryTests.qmd. Reads the two panels 20-RunRegression writes and estimates
# the tables that test the paper's claims as claims, rather than the tables the submitted draft
# happened to contain.
#
# WHY A SECOND SCRIPT. 20 reproduces the ten submitted tables so the port can be reconciled. This
# one asks a different question: for each claim in Section 5, what single table establishes or
# refutes it? Where the draft has three tables twenty pages apart, this has one with three panels;
# where the draft splits a sample to show a shift, this puts a coefficient on the shift; where the
# draft says "untabulated", this tabulates.
#
# Every estimation call goes through reg_fit() from 20, so a specification here and its cousin
# there are fitted by identical code. Functions carry the `clm_` prefix.

# 0. Vocabulary ----------------------------------------------------------------------------------------

# Litigation-risk industries after Francis, Philbrick and Schipper (1994): biotechnology, computer
# hardware, electronics, retail and software. Four-digit SIC ranges, inclusive.
.clm_lit_sic <- list(
  c(2833L, 2836L),
  c(3570L, 3577L),
  c(3600L, 3674L),
  c(5200L, 5961L),
  c(7370L, 7374L)
)

# The three incentives, in the paper's order, and the printed name of each.
.clm_incentives <- c(Fluidity = "competition", Loss = "bad news", ChgSalesQ4Win = "disclosure benefit")

# Labels this document adds to 20's dictionary. Interactions inherit both sides' labels from
# fixest, so only the components need naming.
.reg_dict <- c(
  .reg_dict,
  PostFast = "Post-FAST",
  LitRisk  = "Litigation-risk industry",
  YQ       = "Year-quarter",
  Is8K     = "On an 8-K"
)

# 1. Inputs ----------------------------------------------------------------------------------------------

#' Read the two panels and add the one variable 20 does not carry
#'
#' `LitRisk` is built here rather than in 20 because 20 reproduces the draft and the draft has no
#' litigation-risk variable. It goes on both panels from `Sic4`, which both carry.
#'
#' @param .path_quarter Character. PanelQuarter.parquet from 20.
#' @param .path_contract Character. PanelContract.parquet from 20.
#' @return Named list with `quarter` and `contract`.
clm_read_panels <- function(.path_quarter, .path_contract) {
  if (FALSE) {
    .path_quarter  <- .lP$Input$FilQuarter
    .path_contract <- .lP$Input$FilContract
  }
  lit_ <- function(.sic) {
    hit_ <- rep(FALSE, length(.sic))
    for (r_ in .clm_lit_sic) hit_ <- hit_ | (!is.na(.sic) & .sic >= r_[1L] & .sic <= r_[2L])
    as.integer(hit_)
  }

  quarter_ <- arrow::read_parquet(.path_quarter) |>
    dplyr::mutate(LitRisk = lit_(.data$Sic4))
  contract_ <- arrow::read_parquet(.path_contract) |>
    dplyr::mutate(LitRisk = lit_(.data$Sic4))

  cli::cli_alert_success(
    "quarter {format(nrow(quarter_), big.mark = ',')} rows; contract {format(nrow(contract_), big.mark = ',')} rows"
  )
  lit_q_ <- tbl_pct(mean(quarter_$LitRisk))
  lit_c_ <- tbl_pct(mean(contract_$LitRisk))
  cli::cli_alert_info("litigation-risk industries: {lit_q_} of firm-quarters, {lit_c_} of contracts")
  list(quarter = quarter_, contract = contract_)
}

# 2. Claim A: three incentives, three strategies -------------------------------------------------------

#' One table, three outcomes, the three incentives read down each column
#'
#' The draft estimates filing, delay and redaction in three tables and never puts the incentives
#' side by side. This does. The filing model gains competition and growth, which the draft's
#' firm-quarter specification omitted, so all three columns carry all three incentives.
#'
#' @param .sample_quarter Tibble from `reg_sample_quarter()`.
#' @param .sample_delay Tibble from `reg_sample_contract()` with .min_year = 2005.
#' @param .sample_redact Tibble from `reg_sample_contract()` with .min_year = 2008.
#' @param .firm_fe Logical. Absorb the firm effect (LPM) rather than pooled logit.
#' @return Named list of three models.
clm_table_incentives <- function(.sample_quarter, .sample_delay, .sample_redact, .firm_fe = FALSE) {
  if (FALSE) {
    .sample_quarter <- sample_quarter
    .sample_delay   <- sample_delay
    .sample_redact  <- sample_redact
    .firm_fe        <- FALSE
  }
  inc_  <- names(.clm_incentives)
  rhs_q <- c(inc_, setdiff(.reg_controls$Quarter, inc_))
  rhs_c <- c(inc_, setdiff(.reg_controls$Full, inc_))
  est_  <- if (.firm_fe) "lpm" else "logit"

  fe_q <- if (.firm_fe) c("cyear", "fqtr") else c("cyear", "fqtr", "FfInd", "State")
  fe_c <- if (.firm_fe) c("cyear", "fqtr", "Class") else c("cyear", "fqtr", "FfInd", "Class")

  list(
    Filed    = reg_fit(tidyr::drop_na(.sample_quarter, dplyr::all_of(rhs_q)), .dv = "Filed",
                       .rhs = rhs_q, .fe = fe_q, .estimator = est_, .firm_fe = .firm_fe),
    Delayed  = reg_fit(tidyr::drop_na(.sample_delay, dplyr::all_of(rhs_c)), .dv = "Delayed",
                       .rhs = rhs_c, .fe = fe_c, .estimator = est_, .firm_fe = .firm_fe),
    Redacted = reg_fit(tidyr::drop_na(.sample_redact, dplyr::all_of(c(rhs_c, "Redacted"))), .dv = "Redacted",
                       .rhs = rhs_c, .fe = fe_c, .estimator = est_, .firm_fe = .firm_fe)
  )
}

# 3. Claim B: loss, and the litigation mechanism ---------------------------------------------------------

#' Loss on all three outcomes, interacted with litigation-risk industry
#'
#' The story's pivot is that loss firms cannot omit because non-disclosure of bad news raises
#' litigation exposure. If that is the mechanism, the loss effect on filing is stronger where
#' litigation risk is higher. The interaction is the test; the main effects are Claim A's loss row.
#'
#' @inheritParams clm_table_incentives
#' @return Named list of three models.
clm_table_litigation <- function(.sample_quarter, .sample_delay, .sample_redact) {
  if (FALSE) {
    .sample_quarter <- sample_quarter
    .sample_delay   <- sample_delay
    .sample_redact  <- sample_redact
  }
  rhs_q <- c("Loss:LitRisk", "LitRisk", .reg_controls$Quarter)
  rhs_c <- c("Loss:LitRisk", "LitRisk", .reg_controls$Full, "Fluidity")
  fe_q  <- c("cyear", "fqtr", "State")
  fe_c  <- c("cyear", "fqtr", "Class")

  list(
    Filed    = reg_fit(.sample_quarter, .dv = "Filed", .rhs = rhs_q, .fe = fe_q, .estimator = "logit"),
    Delayed  = reg_fit(tidyr::drop_na(.sample_delay, "Fluidity"), .dv = "Delayed",
                       .rhs = rhs_c, .fe = fe_c, .estimator = "logit"),
    Redacted = reg_fit(tidyr::drop_na(.sample_redact, dplyr::all_of(c("Fluidity", "Redacted"))), .dv = "Redacted",
                       .rhs = rhs_c, .fe = fe_c, .estimator = "logit")
  )
}

# 4. Claims C and E: what the FAST Act changed --------------------------------------------------------------

#' An incentive's effect on an outcome, and how much of it survives the FAST Act
#'
#' The draft shows a shift by splitting the sample and pointing at two columns. This puts a
#' coefficient on the shift: each named term enters as a main effect and as an interaction with
#' `PostFast`, and year-quarter fixed effects absorb `PostFast` itself along with any trend. The
#' post-FAST effect of a term is the main effect plus its interaction; a shift is an interaction
#' with a standard error.
#'
#' @param .sample Tibble from `reg_sample_contract()`.
#' @param .dv Character. The outcome.
#' @param .terms Character. Which regressors to interact with PostFast.
#' @param .firm_fe Logical.
#' @return A single model.
clm_fit_fast_shift <- function(.sample, .dv, .terms, .firm_fe = FALSE) {
  if (FALSE) {
    .sample  <- sample_redact
    .dv      <- "Redacted"
    .terms   <- "Fluidity"
    .firm_fe <- FALSE
  }
  base_ <- unique(c(.reg_controls$Full, "Fluidity"))
  rhs_  <- c(.terms, paste0(.terms, ":PostFast"), setdiff(base_, .terms))
  fe_   <- if (.firm_fe) c("YQ", "Class") else c("YQ", "FfInd", "Class")
  dat_  <- tidyr::drop_na(.sample, dplyr::all_of(c(.dv, base_)))

  reg_fit(dat_, .dv = .dv, .rhs = rhs_, .fe = fe_, .estimator = if (.firm_fe) "lpm" else "logit",
          .firm_fe = .firm_fe)
}

#' Claim C (competition under review) and Claim E (every incentive) on redaction, side by side
#'
#' @param .sample Tibble from `reg_sample_contract()` with .min_year = 2008.
#' @return Named list of four models: competition alone, pooled and firm FE; all incentives, both.
clm_table_fast_shift <- function(.sample) {
  if (FALSE) .sample <- sample_redact
  all_ <- c("Fluidity", "Loss", "ChgSalesQ4Win", "LeverageWin")
  list(
    `Competition`         = clm_fit_fast_shift(.sample, .dv = "Redacted", .terms = "Fluidity", .firm_fe = FALSE),
    `Competition, firm FE` = clm_fit_fast_shift(.sample, .dv = "Redacted", .terms = "Fluidity", .firm_fe = TRUE),
    `All incentives`      = clm_fit_fast_shift(.sample, .dv = "Redacted", .terms = all_, .firm_fe = FALSE),
    `All, firm FE`        = clm_fit_fast_shift(.sample, .dv = "Redacted", .terms = all_, .firm_fe = TRUE)
  )
}

# 5. Claim D: the 2004 reform, cleanly ------------------------------------------------------------------------

#' The continuous DiD on voluntary items, restricted to quarters that have a contract 8-K
#'
#' Three changes from the draft's Table 8 and from 20's reproduction of it. The outcome counts the
#' pre-reform items under their old numbers. Only firm-quarters with at least one contract on an
#' 8-K enter, so a quarter whose contracts all sat on a 10-K is not a zero -- it is absent. And
#' year-quarter fixed effects replace the quadratic trend, absorbing `Post` and leaving only the
#' interaction identified, which is the difference-in-differences.
#'
#' The treatment share is computed over ALL of a firm's pre-reform contracts, 10-K ones included,
#' because that is what "share delayed" means; the restriction to 8-K quarters applies to the
#' outcome only.
#'
#' @param .contract Tibble, the contract panel.
#' @return Named list of three models: year-quarter FE only, plus industry, plus firm.
clm_table_did_clean <- function(.contract) {
  if (FALSE) .contract <- panels$contract

  reform_ <- as.Date("2004-08-23")
  ctl_    <- c(.reg_controls$Full, "Fluidity")

  share_ <- .contract |>
    dplyr::filter(.data$DateFiled < reform_) |>
    dplyr::summarise(ShareDelayedPre = mean(.data$Delayed), .by = "gvkey")

  dat_ <- .contract |>
    dplyr::filter(.data$Is8K, .data$fyear < 2008L) |>
    dplyr::summarise(
      nItemsVoluntary  = mean(.data$nItemsVoluntary, na.rm = TRUE),
      HasVoluntaryItem = as.integer(any(.data$HasVoluntaryItem == 1L)),
      datadate         = dplyr::first(.data$datadate),
      YQ               = dplyr::first(.data$YQ),
      dplyr::across(dplyr::all_of(c(ctl_, "FfInd")), dplyr::first),
      .by = c("gvkey", "fyear", "fqtr")
    ) |>
    dplyr::inner_join(share_, by = "gvkey") |>
    tidyr::drop_na(dplyr::all_of(c("ShareDelayedPre", "nItemsVoluntary", ctl_))) |>
    dplyr::mutate(
      Post2004 = as.integer(.data$datadate >= reform_),
      Did      = .data$Post2004 * .data$ShareDelayedPre
    )

  cli::cli_alert_info(
    "{format(nrow(dat_), big.mark = ',')} firm-quarters with a contract 8-K, {dplyr::n_distinct(dat_$gvkey)} firms"
  )

  rhs_ <- c("Did", "ShareDelayedPre", ctl_)
  list(
    `Year-quarter` = reg_fit(dat_, .dv = "nItemsVoluntary", .rhs = rhs_, .fe = "YQ", .estimator = "lpm"),
    `+ Industry`   = reg_fit(dat_, .dv = "nItemsVoluntary", .rhs = rhs_, .fe = c("YQ", "FfInd"), .estimator = "lpm"),
    `+ Firm`       = reg_fit(dat_, .dv = "nItemsVoluntary", .rhs = setdiff(rhs_, "ShareDelayedPre"),
                             .fe = "YQ", .estimator = "lpm", .firm_fe = TRUE),
    `Bundled, firm` = reg_fit(dat_, .dv = "HasVoluntaryItem", .rhs = setdiff(rhs_, "ShareDelayedPre"),
                              .fe = "YQ", .estimator = "lpm", .firm_fe = TRUE)
  )
}

# 6. Claim F: redaction and delay together ---------------------------------------------------------------------

#' Are the two levers complements or substitutes at the contract level?
#'
#' Delay on redaction, within firm, with the FAST interaction. Positive in both periods means a
#' redacting firm also delays -- complements, which is what the visibility argument predicts. A
#' negative interaction means that once redaction became cheap, firms that took it needed delay
#' less -- substitution at the margin.
#'
#' @param .sample Tibble from `reg_sample_contract()` with .min_year = 2008, so Redacted is defined.
#' @return Named list of two models.
clm_table_complements <- function(.sample) {
  if (FALSE) .sample <- sample_redact
  dat_ <- tidyr::drop_na(.sample, dplyr::all_of(c("Redacted", "Fluidity")))
  rhs_ <- c("Redacted", "Redacted:PostFast", .reg_controls$Full, "Fluidity")
  list(
    Pooled    = reg_fit(dat_, .dv = "Delayed", .rhs = rhs_, .fe = c("YQ", "FfInd", "Class"), .estimator = "logit"),
    `Firm FE` = reg_fit(dat_, .dv = "Delayed", .rhs = rhs_, .fe = c("YQ", "Class"), .estimator = "lpm", .firm_fe = TRUE)
  )
}

# 7. Reports -------------------------------------------------------------------------------------------------------

#' The scorecard: each claim's test coefficient, its sign, and whether it confirms
#'
#' Reads the coefficient of interest out of each model list and states the verdict the design
#' document pre-committed to. A report function: prints, returns invisibly.
#'
#' @param .inc Models from `clm_table_incentives()`.
#' @param .lit Models from `clm_table_litigation()`.
#' @param .fast Models from `clm_table_fast_shift()`.
#' @param .did Models from `clm_table_did_clean()`.
#' @param .comp Models from `clm_table_complements()`.
#' @return Invisibly, the scorecard tibble.
clm_report_scorecard <- function(.inc, .lit, .fast, .did, .comp) {
  if (FALSE) {
    .inc  <- tab_incentives
    .lit  <- tab_litigation
    .fast <- tab_fast
    .did  <- tab_did
    .comp <- tab_complements
  }
  get_ <- function(.m, .term) {
    ct_ <- fixest::coeftable(.m)
    i_  <- which(rownames(ct_) == .term)
    if (length(i_) == 0L) return(c(NA_real_, NA_real_))
    c(ct_[i_, 1L], ct_[i_, 4L])
  }
  row_ <- function(.claim, .test, .m, .term, .pred) {
    v_ <- get_(.m, .term)
    ok_ <- if (is.na(v_[1L])) NA else switch(.pred,
      `+` = v_[1L] > 0 && v_[2L] < 0.05,
      `-` = v_[1L] < 0 && v_[2L] < 0.05,
      `0` = v_[2L] >= 0.05
    )
    tibble::tibble(Claim = .claim, Test = .test, Predicted = .pred, Coef = v_[1L], P = v_[2L],
                   Verdict = dplyr::case_when(is.na(ok_) ~ "absent", ok_ ~ "confirms", .default = "does not"))
  }
  tab_ <- dplyr::bind_rows(
    row_("A", "competition -> filed",    .inc$Filed,    "Fluidity",             "-"),
    row_("A", "competition -> delayed",  .inc$Delayed,  "Fluidity",             "+"),
    row_("A", "competition -> redacted", .inc$Redacted, "Fluidity",             "+"),
    row_("A", "loss -> filed",           .inc$Filed,    "Loss",                 "+"),
    row_("A", "loss -> delayed",         .inc$Delayed,  "Loss",                 "+"),
    row_("A", "loss -> redacted",        .inc$Redacted, "Loss",                 "+"),
    row_("A", "growth -> filed",         .inc$Filed,    "ChgSalesQ4Win",        "+"),
    row_("A", "growth -> delayed",       .inc$Delayed,  "ChgSalesQ4Win",        "-"),
    row_("A", "growth -> redacted",      .inc$Redacted, "ChgSalesQ4Win",        "-"),
    row_("B", "loss x litigation -> filed", .lit$Filed, "Loss:LitRisk",         "+"),
    row_("C", "competition x post-FAST -> redacted", .fast$Competition, "Fluidity:PostFast", "-"),
    row_("E", "leverage x post-FAST -> redacted", .fast$`All incentives`, "LeverageWin:PostFast", "+"),
    row_("D", "share delayed x post -> voluntary items", .did$`+ Firm`, "Did",  "+"),
    row_("F", "redacted -> delayed",     .comp$Pooled,  "Redacted",             "+"),
    row_("F", "redacted x post-FAST -> delayed", .comp$Pooled, "Redacted:PostFast", "0")
  )
  tbl_out(tab_, .title = "Scorecard: pre-committed predictions against the estimates", .digits = 3L)
  invisible(tab_)
}
