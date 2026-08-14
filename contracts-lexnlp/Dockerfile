# syntax=docker/dockerfile:1
# NO PLATFORM PIN. This image was pinned to linux/amd64 because two of LexNLP's transitive
# dependencies fall back to source on arm64 and the slim base carries no compiler -- the pin
# treated a missing toolchain as a missing wheel. On an Apple Silicon host that meant every
# extraction ran under Rosetta, which is a 20-40% tax on CPU-bound Python, and LexNLP's company
# NER is 74% of the corpus bill. The toolchain is added and removed below instead.
FROM python:3.8-slim
# Unbuffered so the tqdm bar + summary stream straight through to the caller (R).
ENV PYTHONUNBUFFERED=1
# NATIVE arm64, no Rosetta. Every LexNLP dependency publishes an aarch64 wheel -- numpy, pandas,
# scipy, scikit-learn, gensim, lxml, regex all resolve natively. Two TRANSITIVE packages do not:
# psutil 5.9.4 ships an arm64 wheel for macOS only, and backports.zoneinfo (pulled in by dateparser
# for Python < 3.9) publishes no aarch64 wheel at all. Both are small C extensions that compile in
# seconds given a compiler, which python:3.8-slim does not carry.
#
# gcc goes in and comes out in the same layer, so the toolchain is never committed to the image.
RUN apt-get update \
 && apt-get install -y --no-install-recommends gcc python3-dev \
 && pip install --no-cache-dir lexnlp==2.3.0 pyarrow tqdm \
 && apt-get purge -y --auto-remove gcc python3-dev \
 && rm -rf /var/lib/apt/lists/*
# Bake the NLTK data LexNLP's NER needs into the image -> runtime is offline + reproducible.
RUN python -m nltk.downloader -d /usr/share/nltk_data \
      maxent_ne_chunker punkt words averaged_perceptron_tagger
ENV NLTK_DATA=/usr/share/nltk_data
WORKDIR /work
COPY app/geoentities.csv /app/geoentities.csv
COPY extract_lexnlp.py /app/extract_lexnlp.py
ENTRYPOINT ["python", "/app/extract_lexnlp.py"]