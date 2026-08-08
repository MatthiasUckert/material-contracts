# Run each step separately with timing. Expected: step 1 ~10-30s, step 2 a few seconds,
# step 3 ~30s, step 4 ~1-2 min. Whichever one does not return is the problem.

con <- ent_session(.db_path = .lP$Input$Store, .keys = tab_keys)

cat("\n[1] DISTINCT spans\n"); print(system.time(
  DBI::dbExecute(con, "CREATE OR REPLACE TABLE spans AS
     SELECT DISTINCT DocID, Label, Start, Stop, Span FROM s.candidates WHERE Start IS NOT NULL")
))
print(DBI::dbGetQuery(con, "SELECT Label, COUNT(*) N FROM spans GROUP BY 1 ORDER BY 1"))

cat("\n[2] distinct DATE strings\n"); print(system.time(
  d <- DBI::dbGetQuery(con, "SELECT DISTINCT Span FROM spans WHERE Label = 'DATE'")
))
cat("distinct date strings:", nrow(d), "\n")
cat("with a plausible 4-digit year:",
    sum(stringi::stri_detect_regex(d$Span, "(19|20)[0-9]{2}")), "\n")

cat("\n[3] anytime on the first 5,000 only\n"); print(system.time(
  suppressWarnings(anytime::anydate(utils::head(d$Span, 5000)))
))
cat("--> multiply that by", round(nrow(d)/5000, 1), "for the full set\n")

cat("\n[4] registered-frame join cost\n"); print(system.time(
  DBI::dbGetQuery(con, "SELECT COUNT(*) FROM spans sp JOIN keys k USING (DocID)")
))

DBI::dbDisconnect(con, shutdown = TRUE)
