#!/usr/bin/env python3
"""
Phase C - Theme 4 (data-generation). Audience reception from the comment corpus
(2,173 comments over 436 posts, 15-comment/post cap): BERTopic topics, per-comment
VADER sentiment and activity hour, the commenter network (1,225 unique usernames),
and the org shared-audience projection. Reply behaviour is checked but the scrape
carries no reply payloads (all null), so org-reply behaviour is not analysable here.

Comment timestamps are UTC; Namibia local time is UTC+2 (hours reported as UTC).

Reads : ../2. Data Collection (Apify)/Comment Data Apify/comments_merged.json,
        analysis_base.csv
Writes: output/comment_topics_info.csv, output/comments_labeled.csv,
        output/commenter_summary.csv, output/commenter_org_edges.csv,
        output/org_shared_audience_edges.csv
Run   : "../4. NLP Pipeline/.venv/bin/python" c6_comment_features.py
"""
#
# PATHS IN THIS PACKAGE
# This script was written inside the thesis working tree, where its inputs sat in
# sibling folders named "2. Data Collection (Apify)" and the like. In the
# reproduction package the same files live under data/. Import python/_paths.py
# for the canonical locations rather than editing the constants below:
#
#     import sys; sys.path.insert(0, "python")
#     from _paths import ANALYSIS_BASE, RAW_CORPUS, feature
#
# Status: RECORD ONLY. It reads the raw Apify comment export, a JSON the package
# does not ship. Its outputs, comments_labeled.csv and the four beside it, are
# included and are what the R side reads.
#
# WHY comments: they are the audience's VOICE, the one place we see reception rather
# than production. This script asks what commenters talk about (BERTopic themes), how
# positive/negative they sound (VADER), when they are active, and whether the same
# people show up across multiple orgs (a shared-audience network). All exploratory.
import os, re, json, warnings, ssl
from collections import defaultdict, Counter
from itertools import combinations   # for enumerating org pairs sharing a commenter
import numpy as np
import pandas as pd
warnings.filterwarnings("ignore")    # silence the many library FutureWarnings from BERTopic/UMAP/sklearn

# ensure the VADER lexicon is available (one-time, cached thereafter)
# VADER is a rule/lexicon sentiment tool tuned for social-media text (emoji, caps,
# slang), which is why it suits Instagram comments better than a formal classifier.
try:
    ssl._create_default_https_context = ssl._create_unverified_context   # relax cert check only for the NLTK fetch
except Exception:
    pass
import nltk
try:
    nltk.data.find("sentiment/vader_lexicon")
except LookupError:
    nltk.download("vader_lexicon", quiet=True)

HERE = os.path.dirname(os.path.abspath(__file__))

# Package layout. _paths.py knows where the corpus, the NLP outputs and the feature
# tables live in this package, so no constant below has to name a path that only
# resolves on the author's machine.
import sys as _sys
_here = os.path.dirname(os.path.abspath(__file__))
# Walk up until _paths.py appears, so moving a script between subfolders does not
# break the import and no fixed number of ".." has to be kept in sync.
while _here != os.path.dirname(_here) and not os.path.exists(os.path.join(_here, "_paths.py")):
    _here = os.path.dirname(_here)
_sys.path.insert(0, _here)
from _paths import ANALYSIS_BASE as _BASE, RAW as _RAW, FEATURES as _FEAT

UP = os.path.normpath(os.path.join(HERE, ".."))
OUT = str(_FEAT)
os.makedirs(OUT, exist_ok=True)

# Pull the post shortcode out of an Instagram URL (".../p/<CODE>/") = our Post_ID.
# Returns None if the URL has no /p/<code>/ segment.
def shortcode(u):
    m = re.search(r"/p/([^/]+)/", u or "")
    return m.group(1) if m else None

# ---- load ---------------------------------------------------------------------
# The raw scraped comments (a JSON list) plus the coded corpus, so each comment can be
# linked to the post's org / actor and flagged as in-corpus.
COM = json.load(open(os.path.join(UP, "2. Data Collection (Apify)/Comment Data Apify/comments_merged.json"), encoding="utf-8"))
base = pd.read_csv(str(_BASE), sep=";", encoding="utf-8-sig")
coded = base[base["coded"] == 1]
id2org = dict(zip(coded["Post_ID"], coded["Handle"].astype(str).str.lower()))   # Post_ID -> org handle
id2actor = dict(zip(coded["Post_ID"], coded["Actor_Name"]))                     # Post_ID -> actor display name

# reply-behaviour check (documented, not analysed)
# The scrape did not capture reply threads, so we cannot study whether orgs reply to
# commenters. This just documents that gap in the console for transparency.
n_with_replies = sum(1 for c in COM if c.get("replies"))
n_replycount = sum(1 for c in COM if c.get("repliesCount"))
print(f"Reply-behaviour check: {n_with_replies}/{len(COM)} comments carry reply payloads, "
      f"{n_replycount} carry a repliesCount -> org-reply behaviour NOT analysable from this scrape.")

from nltk.sentiment.vader import SentimentIntensityAnalyzer
sia = SentimentIntensityAnalyzer()   # the VADER scorer (compound score in [-1, +1])

# Flatten each comment into a tidy record: identity, text + length, VADER compound
# sentiment, like count, and UTC hour/weekday of posting.
rows = []
for c in COM:
    pid = shortcode(c.get("postUrl"))
    if pid is None:
        continue                                        # skip comments we cannot tie to a post
    txt = (c.get("text") or "").strip()
    ts = pd.to_datetime(c.get("timestamp"), errors="coerce", utc=True)   # parse to UTC (NaT if missing)
    rows.append({
        "comment_id": c.get("id"), "Post_ID": pid,
        "src_org": id2org.get(pid, ""), "actor": id2actor.get(pid, ""),
        "commenter": (c.get("ownerUsername") or "").lower(),
        "text": txt, "text_len": len(txt),
        "vader": sia.polarity_scores(txt)["compound"] if txt else 0.0,   # compound sentiment; 0 for empty text
        "likesCount": c.get("likesCount") or 0,
        "hour_utc": int(ts.hour) if pd.notna(ts) else np.nan,   # activity hour (UTC; Namibia = UTC+2)
        "weekday": int(ts.weekday()) if pd.notna(ts) else np.nan,
        "in_corpus": int(pid in id2org),                # 1 if the comment belongs to a coded post
    })
cdf = pd.DataFrame(rows)
print(f"Comments parsed: {len(cdf)} | joined to coded posts: {int(cdf.in_corpus.sum())} "
      f"over {cdf[cdf.in_corpus==1].Post_ID.nunique()} posts")

# ---- BERTopic over comment text -----------------------------------------------
# Unsupervised topic model: embed each comment, reduce dimensions (UMAP), cluster
# (HDBSCAN), then label clusters by their distinctive words (c-TF-IDF). Reveals what
# themes recur in the comments without a predefined codebook.
from sentence_transformers import SentenceTransformer
from bertopic import BERTopic
from umap import UMAP
from hdbscan import HDBSCAN
from sklearn.feature_extraction.text import CountVectorizer

docs_df = cdf[cdf.text_len >= 3].copy().reset_index(drop=True)   # drop near-empty comments (too short to embed meaningfully)
docs = docs_df["text"].tolist()
print(f"\nBERTopic input: {len(docs)} comments with text (>=3 chars)")
embedder = SentenceTransformer("all-MiniLM-L6-v2")   # compact sentence-embedding model
emb = embedder.encode(docs, show_progress_bar=False, batch_size=64)   # comment -> dense vector
# UMAP: reduce embeddings to 5D before clustering. random_state=42 -> reproducible layout.
umap_model = UMAP(n_neighbors=15, n_components=5, min_dist=0.0, metric="cosine", random_state=42)
# HDBSCAN: density clustering; min_cluster_size=12 = a topic needs >=12 comments.
# Points in no dense region are labelled -1 (outliers/noise), not forced into a topic.
hdbscan_model = HDBSCAN(min_cluster_size=12, metric="euclidean", cluster_selection_method="eom", prediction_data=True)
# CountVectorizer feeds BERTopic's per-topic keyword scoring: 1-2 word terms, English
# stop-words removed, and a term must appear in >=3 comments (min_df) to count.
vectorizer = CountVectorizer(stop_words="english", ngram_range=(1, 2), min_df=3)
topic_model = BERTopic(embedding_model=embedder, umap_model=umap_model, hdbscan_model=hdbscan_model,
                       vectorizer_model=vectorizer, calculate_probabilities=False, verbose=False)
topics, _ = topic_model.fit_transform(docs, emb)     # assign a topic id to each comment (reusing our own embeddings)
docs_df["topic"] = topics

# Build the topic-info table and add a readable "top_words" column per topic.
info = topic_model.get_topic_info()
def topwords(tid):
    t = topic_model.get_topic(tid)
    return ", ".join(w for w, _ in t[:8]) if t else ""   # the 8 highest-scoring keywords for the topic
info["top_words"] = info["Topic"].map(topwords)
info = info.rename(columns={"Topic": "topic", "Count": "count", "Name": "name",
                            "Representative_Docs": "rep_docs"})
info[["topic", "count", "name", "top_words", "rep_docs"]].to_csv(
    os.path.join(OUT, "comment_topics_info.csv"), index=False)

# Attach each comment's topic back onto the full comment table and save it (dropping
# the raw text column to keep the CSV lean / less identifying). Int64 keeps the -1
# outlier label and NaN for comments that were too short to model.
cdf = cdf.merge(docs_df[["comment_id", "topic"]], on="comment_id", how="left")
cdf["topic"] = cdf["topic"].astype("Int64")
cdf.drop(columns=["text"]).to_csv(os.path.join(OUT, "comments_labeled.csv"), index=False)

# ---- commenter network --------------------------------------------------------
# Restrict to named commenters on in-corpus posts, then summarise each commenter:
# how many comments / posts / distinct orgs they engaged, and their mean sentiment.
# n_orgs > 1 marks people who follow MULTIPLE conservation orgs (shared audience).
cc = cdf[(cdf.commenter != "") & (cdf.in_corpus == 1)]
csum = (cc.groupby("commenter")
        .agg(n_comments=("comment_id", "size"), n_posts=("Post_ID", "nunique"),
             n_orgs=("src_org", "nunique"), mean_vader=("vader", "mean")).reset_index()
        .sort_values(["n_orgs", "n_comments"], ascending=False))
csum["orgs_list"] = csum["commenter"].map(
    cc.groupby("commenter")["src_org"].apply(lambda s: ";".join(sorted(set(s)))))   # the specific orgs each commenter touched
csum.to_csv(os.path.join(OUT, "commenter_summary.csv"), index=False)

# Bipartite edges commenter<->org (weight = number of comments to that org).
edges = (cc.groupby(["commenter", "src_org"]).size().reset_index(name="weight"))
edges.to_csv(os.path.join(OUT, "commenter_org_edges.csv"), index=False)

# org shared-audience projection (orgs linked by shared commenters)
# Project the commenter<->org graph onto orgs: for every commenter, connect each pair
# of orgs they commented on; the weight is how many DISTINCT commenters two orgs share.
comm_orgs = cc.groupby("commenter")["src_org"].apply(lambda s: sorted(set(s)))   # each commenter -> the set of orgs they touched
pair = Counter()
for orgs in comm_orgs:
    for a, b in combinations(orgs, 2):   # every unordered pair of orgs this commenter links
        pair[(a, b)] += 1
shared = pd.DataFrame([(a, b, w) for (a, b), w in pair.items()],
                      columns=["org_a", "org_b", "shared_commenters"]).sort_values("shared_commenters", ascending=False)
shared.to_csv(os.path.join(OUT, "org_shared_audience_edges.csv"), index=False)

# ---- audit --------------------------------------------------------------------
# Console summary: topic + outlier counts, commenter-network sizes, sentiment split,
# and the strongest cross-org commenters / shared-audience pairs.
n_topics = int((info.topic >= 0).sum())                          # real topics (topic id -1 = outliers)
outlier = int(info.loc[info.topic == -1, "count"].sum()) if (info.topic == -1).any() else 0
print(f"\nc6_comment_features.py:")
print(f"  BERTopic: {n_topics} topics + {outlier} outliers -> comment_topics_info.csv")
print(f"  per-comment labels -> comments_labeled.csv ({len(cdf)} rows)")
print(f"  commenters: {csum.shape[0]} unique | repeat (>=2 comments): {int((csum.n_comments>=2).sum())} "
      f"| cross-org (>=2 orgs): {int((csum.n_orgs>=2).sum())}")
print(f"  org shared-audience edges: {len(shared)}")
print("\nTop comment topics:")
print(info[info.topic >= 0].head(10)[["topic", "count", "top_words"]].to_string(index=False))
# VADER convention: compound >= .05 positive, <= -.05 negative, in-between neutral.
print("\nComment sentiment (VADER): mean {:.3f} | pos {:.0f}% neu {:.0f}% neg {:.0f}%".format(
    cc.vader.mean(),
    100*(cc.vader >= 0.05).mean(), 100*((cc.vader > -0.05) & (cc.vader < 0.05)).mean(),
    100*(cc.vader <= -0.05).mean()))
print("\nMost cross-org commenters:")
print(csum.head(6)[["commenter", "n_comments", "n_posts", "n_orgs"]].to_string(index=False))
print("\nTop org shared-audience pairs:")
print(shared.head(8).to_string(index=False))