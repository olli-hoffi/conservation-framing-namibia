# Python Reproduction Code

Python side of the reproduction package for the MSc thesis *Message Framing on
Instagram: A Content Analysis and Naturalistic Comparison of Conservation
Communication in Namibia* (Oliver Hoffmann, Goethe University Frankfurt).

These scripts do the **data generation and collection** that feeds the R
analysis (`../analysis/`): they scrape and reshape the Study 1 corpus, produce
the NLP frame and topic labels, build the Study 1 exploratory feature tables,
and build / query the Study 2 Meta Ads campaign. All inferential statistics and
the final thesis tables and figures live on the R side; Python stops at the
feature and raw-data layer.

Unlike the R scripts, the Python was **kept as-is** (not restyled): these are the
original working scripts, so their results match the thesis exactly.

## Where the inputs live

`_paths.py` holds the canonical location of every input, as an absolute path that
resolves from any working directory:

```python
import sys; sys.path.insert(0, "python")
from _paths import RAW_CORPUS, ANALYSIS_BASE, feature
```

The scripts were written inside the thesis working tree, where these files sat in
sibling folders named `2. Data Collection (Apify)` and the like. In this package
they live under `data/`, and four feature scripts are wired to `_paths.py` and run
against the shipped data as they stand.

```bash
python3 -m venv .venv && source .venv/bin/activate
pip install -r python/requirements.txt
cd python/study1/4_features && python3 c2_temporal_features.py
```

The requirements file installs the embedding and topic-model packages as well, which
pulls torch and takes roughly two gigabytes. Only `study1_nlp_pipeline.py` needs them.
Everything else runs on the light half of the file, so drop the last block if the
topic model is not what you came for.

Verified with the pinned versions: rerunning `b2`, `c2`, `c3` and `c5` reproduces
every file each writes byte for byte, and each is byte-identical across two
consecutive runs. `study1_nlp_pipeline.py` reproduces the cluster id of all 920 posts
from the shipped embedding cache.

The pins are load-bearing. Installed against spaCy 3.8.16 instead of the recorded
3.8.13, `c5` finds slightly different named entities and four rows of
`entities_ner_orgs_long.csv` change. `requirements.txt` therefore pins exact
versions, including the spaCy model, in the way `renv.lock` does for R.

Every script says in its header where its inputs are and whether it runs offline,
so nobody has to work that out from a stack trace.

Ten scripts are marked **record only** and say why in their own header. The
script map below gives the reason for each. The outputs all ten produced are
shipped and are the canonical record, so nothing in the reported results depends
on rerunning them.

## What the Python side is for, and why it exists at all

R does the inference. Python does the measurement that comes before it.

Study 1 starts as 920 scraped Instagram posts with a caption, a format and two
engagement counts. Nothing in that is a variable an analysis can use. Turning
"a caption" into hashtag counts, emoji counts, readability, named entities,
sentiment, topic assignments and frame codes is what these scripts do. They write
`data/study1/analysis_base.csv`, the 65-column master table, and the 54 feature
tables beside it. The R scripts then read those and never recompute them.

So the split is not stylistic. Python is where the corpus becomes measurable, R is
where the measurements become findings. Deleting the Python side would leave the
65 columns unexplained.

## Folder layout

```
python/
├── _paths.py                 where every input lives, imported by the scripts
├── requirements.txt          pinned dependencies, the counterpart to renv.lock
│
└── study1/                   every script here belongs to Study 1
    │
    ├── 1_collect/     1      the corpus is created
    │   └── convert_apify_to_raw.py     Apify JSON export -> the RAW post CSV
    │
    ├── 2_text_model/  1      the corpus is read by machine
    │   └── study1_nlp_pipeline.py      BERTopic topic model over the captions,
    │                                   blind-coding worksheet
    │
    ├── 3_llm_coding/  1      the corpus is coded
    │   └── code_all_posts_multimodal.py  the multimodal LLM pass over all 920
    │                                   posts, caption plus cover image, 19 variables
    │
    └── 4_features/   15      the corpus becomes measurable
        ├── 00_build_analysis_base.py   joins every source into the master table
        ├── a2, a3                      CLIP image and image-caption features
        ├── a5, a6, a7                  codebook blind spots, sampling frame, and
        │                               the seeded draw of the 150-post subsample
        ├── b1, b2                      the inductive taxonomy pass and its features
        └── c1 … c7                     per-theme features: organization, time,
                                        caption, image, entities, comments
```

Everything here belongs to Study 1, and the folder numbers are its pipeline: the
corpus is collected, read by a text model, coded, and turned into features. Study 2
needs no counterpart. Its outcomes are platform counters that Meta and Google
Analytics deliver as numbers already, so there is nothing to extract from text.

Within `4_features/` the prefixes continue the order. `00` builds the table every
later script joins onto, `a*` works on images and sampling, `b*` on the inductive
taxonomy, `c*` on the per-theme features.

## Script map

| Script | What it produces | Runs offline |
|---|---|---|
| `1_collect/convert_apify_to_raw.py` | Reshapes the Apify export into the RAW post CSV | no, needs the raw Apify JSON, not shipped |
| `2_text_model/study1_nlp_pipeline.py` | BERTopic clusters behind the sampling strata, blind-coding worksheet | yes, with the embedding and topic-model packages of `requirements.txt` |
| `3_llm_coding/code_all_posts_multimodal.py` | The multimodal LLM pass, 19 variables per post from caption and cover image | no, needs a language-model CLI and network access |
| `4_features/00_build_analysis_base.py` | Joins every source into analysis_base.csv, the 65-column master table | no, needs the NLP framing output and the raw Apify JSON |
| `4_features/a2_clip_image_text_alignment.py` | CLIP cosine between caption and cover image | no, needs the post images, not shipped |
| `4_features/a3_clip_visual_clusters.py` | CLIP visual clustering, the zero-shot visual genre | no, needs the post images, not shipped |
| `4_features/a5_codebook_blindspots.py` | Posts the a priori codebook does not describe | **yes** |
| `4_features/a6_sampling_frame.py` | Builds the stratified sampling frame | **yes** |
| `4_features/a7_draw_sample_150.py` | The seeded stratified draw of the 150-post subsample | no, its `image_exists` column checks the post images on disk, which are not shipped |
| `4_features/b1_llm_taxonomy_pass.py` | The inductive taxonomy pass over the subsample | no, needs a language-model CLI, and its output is not deterministic |
| `4_features/b2_taxonomy_features.py` | Turns the taxonomy output into post and comment features | **yes** |
| `4_features/c1_org_features.py` | Per-organization feature matrix for the style clusters | **yes** |
| `4_features/c2_temporal_features.py` | Posting cadence, calendar events, month and weekday | **yes** |
| `4_features/c3_caption_features.py` | Caption construction, hashtags, emoji, readability | **yes** |
| `4_features/c4_image_features.py` | Cover-image visual features and near-duplicate detection | no, needs the post images, not shipped |
| `4_features/c5_entity_features.py` | Named entities from captions: species, places, organizations | **yes** |
| `4_features/c6_comment_features.py` | Audience reception over 2,172 comments, topics and sentiment | no, needs the merged comment JSON, and BERTopic |
| `4_features/c7_build_comment_text.py` | Builds the pseudonymised comment-text file | no, needs the merged comment JSON, not shipped |

Seven of the eighteen run here: `a5`, `a6`, `b2`, `c1`, `c2`, `c3` and `c5`. Each
reproduces its feature table byte for byte from the shipped inputs. The other eleven
cannot, and the reason is one of four. Four need the scraped post images, which are
not shipped because they are the organizations' own photographs, 917 files and
291 MB. Two drive a
language-model CLI whose output is not deterministic. Three need the raw Apify
export, which is not shipped because it carries the account name and profile
picture of every commenter. Two need BERTopic and `sentence_transformers`, which
`requirements.txt` does not install because nothing that runs here imports them.

What all eleven produced is shipped and is the canonical record, so no reported
result depends on rerunning them. Their paths still name the collection folders of
the working environment they were written in, which is why they fail with a missing
file rather than a clean message.

## Credentials

The Meta Ads build and the insight pulls ran through a toolkit bound to a personal
advertising account. It is **not part of this package**, because nobody else could
run it and it would only carry credentials. The reported Study 2 numbers come from
the CSV exports collected during the live rounds, in `data/raw/MetaAds/`, and every
analysis reads those.

The Study 1 exploratory `b1_llm_taxonomy_pass.py` needs the local Claude Code
CLI; its LLM outputs are non-deterministic, so the committed `llm_stage1*.csv`
and `taxonomy_synthesis.md` are the canonical record.

## Dependencies

- **2_text_model / 4_features:** `pandas numpy sentence-transformers
  bertopic scikit-learn hdbscan umap-learn nltk` plus (for the CLIP steps)
  `torch` and a CLIP model. Python >= 3.10.
  (`requests`, `python-dotenv`, `PyYAML`, `pytest`).

`4_features/analysis_base.csv` is byte-identical to `data/study1/analysis_base.csv`.
It is kept because `00_build_analysis_base.py` writes it there and `a5` and `a6` read it
from that folder. The copy under `data/` is the one every R script reads.
