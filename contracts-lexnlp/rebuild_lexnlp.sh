#!/usr/bin/env bash
# Rebuild the contracts-lexnlp image and stamp it with the hash of its own sources.
#
# THE STAMP IS THE POINT. Without it nothing records which version of extract_lexnlp.py,
# company_types.csv or geoentities.csv is inside a running image, so an edit to any of them leaves
# extraction attributed -- in the store -- to code that has changed since. image_spec.py --check
# reads the label back and says so.
set -euo pipefail

# Resolve relative to THIS FILE, not to a hard-coded home directory. The previous version began
# `cd ~/RProjects/Projects/material-contracts/contracts-lexnlp`, which meant the script worked on
# exactly one machine and failed confusingly everywhere else, including in CI.
cd "$(dirname "${BASH_SOURCE[0]}")"

SPEC="$(python3 image_spec.py)"
echo "spec hash: ${SPEC}"

# No --platform flag: it breaks image resolution under the containerd store. The arch-mismatch
# warning at runtime is cosmetic and expected.
docker build --label "spec_hash=${SPEC}" -t contracts-lexnlp .

# Smoke test: --help must run, which proves the entry point is alive inside the image.
docker run --rm contracts-lexnlp --help > /dev/null

python3 image_spec.py --check

echo "contracts-lexnlp rebuilt at ${SPEC}"
