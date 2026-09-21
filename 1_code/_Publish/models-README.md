# The classifiers

Six fine-tuned transformer classifiers: three tasks, each at two context lengths. All six start from
Legal-BERT (`nlpaueb/legal-bert-base-uncased`) and were trained on the 4,398 labelled contracts in
`core/Labels.parquet`.

| Archive | Task | Categories | Context | Labelled the published data |
|:--|:--|--:|--:|:--|
| `ClassDetailed_L256.zip` | Contract type (`Class`) | 12 | 256 tokens | yes |
| `ClassDetailed_L512.zip` | Contract type | 12 | 512 tokens | no |
| `ClassBroad_L256.zip` | Broad type from a separate model (`ClassBroad`) | 7 | 256 tokens | yes |
| `ClassBroad_L512.zip` | Broad type from a separate model | 7 | 512 tokens | no |
| `AmendType_L256.zip` | Original or Amended (`AmendType`) | 2 | 256 tokens | yes |
| `AmendType_L512.zip` | Original or Amended | 2 | 512 tokens | no |

**Which model is the paper's.** The paper's contract type is `Class`, from `ClassDetailed_L256`, and
its broad type is `ClassRollup`, the roll-up of `Class`. The two `ClassBroad` models are an additional
classifier trained on the broad labels directly; their output is published as `ClassBroad` for
comparison, and `HierConsistent` marks where it disagrees with the roll-up.

**Selection and deployment.** `deployed.parquet` records the configuration cross-validation selected
for each task, with its macro-F1: 0.882 for the contract type (256 tokens), 0.923 for the separate
broad model (512 tokens) and 0.953 for original against amendment (256 tokens). The published labels
come from the 256-token models for all three tasks, including the broad one.

## What an archive holds

```
<Task>__nlpaueb-legal-bert-base-uncased__TText_L<len>_E<epochs>_B32_LR<rate>_W<weights>_S42__FINAL/
  model/            the model: config.json with the label mapping (id2label), the weights
                    (model.safetensors) and the tokenizer (tokenizer.json, vocab.txt, ...)
  config.json       the training run: data, labels, hyperparameters, software versions
  train_log.parquet the training loss per epoch
  run.log           the run's log
```

`models_index.csv` maps each archive to that folder name. The folder name spells out the
configuration: context length (`L`), epochs (`E`), batch size (`B`), learning rate (`LR`), class
weights (`W1` on, `W0` off) and seed (`S`).

## Using a model

The models were trained with Python 3.12, PyTorch 2.10 and transformers 4.49; recent versions of both
libraries load them. A GPU helps but is not needed.

```bash
pip install torch transformers pandas pyarrow
unzip ClassDetailed_L256.zip
```

```python
import pandas as pd
import torch
from transformers import AutoModelForSequenceClassification, AutoTokenizer

model_dir = "ClassDetailed__nlpaueb-legal-bert-base-uncased__TText_L256_E6_B32_LR2e-05_W1_S42__FINAL/model"
max_len = 256                      # the context length the model was trained with: L256 or L512

tok = AutoTokenizer.from_pretrained(model_dir)
model = AutoModelForSequenceClassification.from_pretrained(model_dir).eval()
id2label = {int(k): v for k, v in model.config.id2label.items()}

docs = pd.read_parquet("text/exhibit10/exhibit10_2012.parquet", columns=["DocID", "TextRaw"]).head(32)
enc = tok(docs["TextRaw"].fillna("").tolist(), truncation=True, max_length=max_len,
          padding="max_length", return_tensors="pt")
with torch.inference_mode():
    probs = torch.softmax(model(**enc).logits, dim=-1)
top = probs.topk(2, dim=-1)

docs["Label"] = [id2label[i] for i in top.indices[:, 0].tolist()]
docs["Prob"] = top.values[:, 0].tolist()
docs["Label2"] = [id2label[i] for i in top.indices[:, 1].tolist()]
docs["Prob2"] = top.values[:, 1].tolist()
```

**Three rules the published labels follow, and your own use should too:**

- **The input is `TextRaw`** from `text/`, the document's full text, unchanged.
- **The model reads the first `max_len` tokens** and ignores the rest (`truncation=True`). Use the
  length in the archive's name; a 256-token model given 512 tokens is not the model that was scored.
- **The label mapping travels with the model** (`id2label` in `model/config.json`); never rebuild it
  from a list of category names.

`Label`, `Prob`, `Label2` and `Prob2` correspond to `BertClassDetailed`, `BertClassDetailedProb`,
`BertClassDetailed2` and `BertClassDetailed2Prob` in `core/Contracts.parquet` (and likewise for the
other two tasks). `text/` holds exactly the text the models read, so a 256-token model run on it
reproduces those columns, up to the last digits of a probability that differ between processors.

To roll a detailed label up to the broad category the paper uses: the part before the colon, with
`Customer / Supplier` and `R&D` rolling up to `Purchases and Sales`, and `Leases`, `Licenses` and
`Other` standing alone.

## Licence

The models are derived from Legal-BERT, published under CC BY-SA 4.0, and are published under the
same licence.
