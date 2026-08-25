# syntax=docker/dockerfile:1
#
# THE BASE IS PINNED BY DIGEST, AND THE DIGEST IS NOT THE ONE THE CORPUS WAS EXTRACTED WITH.
# That distinction is load-bearing and is stated here rather than left to be discovered.
#
# The image that produced the 125 million corpus spans was built on a python:3.8-slim whose digest
# was never recorded: it carries no base.digest label, `docker history` shows <missing> for the base
# layer, and RootFS.Layers gives uncompressed diff IDs that cannot be mapped back to a registry
# manifest. Its base layer is 23 months old and Python 3.8 is end-of-life. There is therefore no
# version of "pin what we have" that can be written.
#
# So two different things carry the two different guarantees:
#
#   REPRODUCIBILITY is the ARCHIVED IMAGE. contracts-lexnlp-17a366b025fe.tar.gz is the exact
#   artifact that extracted the corpus, saved with `docker save` and independent of whether the
#   registry keeps serving a base this old.
#
#   REBUILDABILITY is this file. The digest below is the multi-arch index as of 2026-08-25, so a
#   rebuild from here is deterministic from here ON. It is not, and cannot be shown to be, the same
#   base the corpus used -- and _Tests/Test-LexNLP.R is what answers whether that matters: its span
#   hash over a fixed fixture was 9341e671a38a before this change and must survive it.
#
# THE INDEX DIGEST, NOT A PLATFORM ONE. Pinning the amd64 manifest would force Rosetta on an Apple
# Silicon host, which is the 20-40% tax the platform pin was removed to avoid. The index resolves to
# arm64 here and amd64 elsewhere, and the pin still fixes exactly which bytes either one gets.
FROM python:3.8-slim@sha256:1d52838af602b4b5a831beb13a0e4d073280665ea7be7f69ce2382f29c5a613f
# Unbuffered so the tqdm bar + summary stream straight through to the caller (R).
ENV PYTHONUNBUFFERED=1
# NATIVE arm64, no Rosetta. Every LexNLP dependency publishes an aarch64 wheel -- numpy, pandas,
# scipy, scikit-learn, gensim, lxml, regex all resolve natively. Two TRANSITIVE packages do not:
# psutil 5.9.4 ships an arm64 wheel for macOS only, and backports.zoneinfo (pulled in by dateparser
# for Python < 3.9) publishes no aarch64 wheel at all. Both are small C extensions that compile in
# seconds given a compiler, which python:3.8-slim does not carry.
#
# gcc goes in and comes out in the same layer, so the toolchain is never committed to the image.
#
# THE INSTALL READS THE LOCKFILE, WHICH IT PREVIOUSLY DID NOT. The line here used to be
# `pip install lexnlp==2.3.0 pyarrow tqdm`, so requirements.lock.txt was decorative on both axes at
# once: it could not change the build, and changing it could not move the spec hash. Forty-four pins
# that look like a control and are not one are worse than no lockfile, because a reader of this
# folder reasonably assumes the file governs the build. It does now, and image_spec.SOURCES hashes
# it, so editing it moves the image's identity.
COPY requirements.lock.txt /tmp/requirements.lock.txt
RUN apt-get update \
 && apt-get install -y --no-install-recommends gcc python3-dev \
 && pip install --no-cache-dir -r /tmp/requirements.lock.txt \
 && apt-get purge -y --auto-remove gcc python3-dev \
 && rm -rf /var/lib/apt/lists/* /tmp/requirements.lock.txt
# Bake the NLTK data LexNLP's NER needs into the image -> runtime is offline + reproducible.
RUN python -m nltk.downloader -d /usr/share/nltk_data \
      maxent_ne_chunker punkt words averaged_perceptron_tagger
ENV NLTK_DATA=/usr/share/nltk_data
WORKDIR /work
COPY app/geoentities.csv /app/geoentities.csv
COPY extract_lexnlp.py /app/extract_lexnlp.py
ENTRYPOINT ["python", "/app/extract_lexnlp.py"]
