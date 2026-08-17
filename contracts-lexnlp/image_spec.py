#!/usr/bin/env python3
"""Does the contracts-lexnlp image match the sources it was built from?

    python3 image_spec.py            # print the spec hash of the working tree
    python3 image_spec.py --check    # compare against the built image; exit 1 if stale

WHY THIS EXISTS
The image is built by hand and used for months. Nothing anywhere records which version of
extract_lexnlp.py, company_types.csv or geoentities.csv is inside it, so an edit to any of them
leaves a running image that no longer matches the repository -- and the extraction it produces is
attributed, in the store, to code that has changed since. The failure is silent: the container runs,
the parquet is well formed, the rows are wrong about their own provenance.

This is the same mechanism matcon-extract uses for its extractors, applied to a container. Each
extractor there hashes the constants that determine its output and aborts when the hash moves under
an unchanged MODEL. Here the "constants" are the four files baked into the image, and the hash is
stamped in as a LABEL at build time so `docker image inspect` can read it back.

WHAT IS HASHED, AND WHY EACH
  Dockerfile         the base image, the pins, the build steps
  extract_lexnlp.py  the extractor itself
  company_types.csv  the legal-form vocabulary LexNLP matches against
  app/geoentities.csv the geo entity table

All four change what comes out. Nothing else in the folder does: the probe_*.py scripts are
diagnostics that never run inside the container, and rebuild_lexnlp.* are the build tooling itself.

EXIT CODES
  0  the image matches, or the hash was merely printed
  1  the image is stale, absent, or carries no label -- all three mean "rebuild before trusting it"
"""
from __future__ import annotations

import argparse
import hashlib
import json
import subprocess
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
IMAGE = "contracts-lexnlp"
LABEL = "spec_hash"

#: Every file whose content changes what the container emits. Order is fixed so the hash is stable.
SOURCES = (
    "Dockerfile",
    "extract_lexnlp.py",
    "company_types.csv",
    "app/geoentities.csv",
)


def spec_hash():
    """A 12-character fingerprint of the files baked into the image.

    Hashed as (relative path, sha256 of bytes) pairs rather than as one concatenated blob, so
    renaming a file moves the hash even where its contents are unchanged -- a rename changes what
    the Dockerfile copies.

    :return: the first 12 hex characters of the SHA-256 over that manifest.
    """
    manifest = []
    for rel in SOURCES:
        path = HERE / rel
        if not path.exists():
            raise SystemExit(f"missing source: {rel}")
        manifest.append((rel, hashlib.sha256(path.read_bytes()).hexdigest()))
    blob = json.dumps(manifest, sort_keys=False, ensure_ascii=True)
    return hashlib.sha256(blob.encode("ascii")).hexdigest()[:12]


def image_hash():
    """The spec hash stamped into the built image, or None.

    None covers three distinct situations -- no docker, no image, or an image built before this
    label existed -- and they are deliberately not distinguished, because the answer is the same in
    all three: rebuild before trusting it.
    """
    try:
        out = subprocess.run(
            ["docker", "image", "inspect", IMAGE,
             "--format", "{{ index .Config.Labels \"" + LABEL + "\" }}"],
            capture_output=True, text=True, check=False,
        )
    except FileNotFoundError:
        return None
    if out.returncode != 0:
        return None
    value = out.stdout.strip()
    return value or None


def main(argv=None):
    ap = argparse.ArgumentParser(prog="image_spec", description=__doc__.splitlines()[0])
    ap.add_argument("--check", action="store_true",
                    help="compare against the built image and exit 1 if it is stale")
    a = ap.parse_args(argv)

    want = spec_hash()
    if not a.check:
        print(want)
        return 0

    have = image_hash()
    if have is None:
        print(f"{IMAGE}: no image, no docker, or no {LABEL} label -- rebuild", file=sys.stderr)
        return 1
    if have != want:
        print(f"{IMAGE}: STALE. image={have} sources={want} -- run ./rebuild_lexnlp.sh",
              file=sys.stderr)
        return 1
    print(f"{IMAGE}: current ({want})")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
