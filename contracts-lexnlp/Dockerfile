# linux/amd64 so the old pinned stack resolves from wheels (no arm64 source builds)
FROM --platform=linux/amd64 python:3.8-slim

RUN pip install --no-cache-dir lexnlp==2.3.0 pyarrow tqdm

# Bake NLTK data into the image → runtime is offline + reproducible
RUN python -m nltk.downloader -d /usr/share/nltk_data \
      maxent_ne_chunker punkt words averaged_perceptron_tagger
ENV NLTK_DATA=/usr/share/nltk_data

WORKDIR /work
COPY extract_ner.py /app/extract_ner.py
ENTRYPOINT ["python", "/app/extract_ner.py"]