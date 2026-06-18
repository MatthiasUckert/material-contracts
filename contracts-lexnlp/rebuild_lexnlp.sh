#!/usr/bin/env bash
# Rebuild the contracts-lexnlp image after changing extract_lexnlp.py
set -euo pipefail

cd ~/RProjects/Projects/material-contracts/contracts-lexnlp

# Rebuild (no --platform flag: breaks image resolution under the containerd
# store; the arch-mismatch warning at runtime is cosmetic and expected)
docker build -t contracts-lexnlp .

# Smoke test: the new --help should run and show the script is alive
docker run --rm contracts-lexnlp --help

echo "✅ contracts-lexnlp rebuilt"