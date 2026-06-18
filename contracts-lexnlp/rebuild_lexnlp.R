# Rebuild the contracts-lexnlp Docker image after changing extract_lexnlp.py ----
# Usage: source("contracts-lexnlp/rebuild_lexnlp.R")
#
# Build context = this folder (Dockerfile + extract_lexnlp.py live here); layers
# are cached, so only the final COPY of the script rebuilds (seconds). No
# --platform flag: breaks image resolution under the containerd store; the
# arch-mismatch warning at runtime is cosmetic and expected.

context_dir <- here::here("contracts-lexnlp")
image_name <- "contracts-lexnlp"

if (Sys.which("docker") == "") cli::cli_abort("docker not found on PATH.")
if (!fs::dir_exists(context_dir)) cli::cli_abort("Build context not found: {.path {context_dir}}.")

t0 <- Sys.time()

# Build ----
status <- system2("docker", c("build", "-t", image_name, as.character(context_dir)),
                  stdout = "", stderr = ""
)
if (!identical(as.integer(status), 0L)) cli::cli_abort("docker build failed (status {status}).")

# Smoke test: the freshly baked script must run and answer --help ----
status <- system2("docker", c("run", "--rm", image_name, "--help"),
                  stdout = "", stderr = ""
)
if (!identical(as.integer(status), 0L)) cli::cli_abort("Smoke test failed (status {status}).")

elapsed <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
cli::cli_alert_success("Image {.val {image_name}} rebuilt in {elapsed}s")