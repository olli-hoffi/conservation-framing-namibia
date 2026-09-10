#!/usr/bin/env python3
"""
Phase C - Theme 1 (data-generation). Per-org aggregate feature matrix for the
communication-style typology (clustered in R 03_org_typology.R).

One row per organisation (19 orgs). Aggregates the full coded corpus (N=917) plus
the Apify profile snapshot and the Phase A functional_genre labels. Python =
data-generation; the clustering / silhouette / profiling is done in R.

Reads : analysis_base.csv, features/sampling_frame.csv, Study1_Org_Profiles_2026-07-03.json
Writes: output/org_feature_matrix.csv
Run   : "../4. NLP Pipeline/.venv/bin/python" c1_org_features.py
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
# Status: runs offline against the shipped data. The Apify PROFILE export it reads,
# data/raw/Study1/Study1_Org_Profiles_2026-07-03.json, ships with the package, and
# its output org_feature_matrix.csv reproduces byte for byte.
#
# WHY per-org: the analysis question here is "what distinct communication STYLES do
# Namibian conservation actors use?" To answer it we collapse every post an org made
# into one summary profile (how long the captions, how much video, how emotive the
# imagery, how varied the topics), then hand that 19-row matrix to R for clustering.
# Everything in this file is exploratory description, not a hypothesis test.
import os, glob, json          # os/glob: file paths + wildcard search; json: read the Apify profile dump
import numpy as np             # numeric arrays + NaN handling
import pandas as pd            # the data-frame workhorse for all the group-by aggregation

# Resolve folder paths RELATIVE to this script, so it runs no matter the working dir.
HERE = os.path.dirname(os.path.abspath(__file__))          # the "5. Exploratory Analysis" folder

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
from _paths import ANALYSIS_BASE as _BASE, RAW as _RAW, FEATURES as _FEAT, ORG_PROFILES

UP = os.path.normpath(os.path.join(HERE, ".."))            # its parent (the Study-1 root that holds the sibling phase folders)
OUT = str(_FEAT)                         # where every CSV this script writes lands
os.makedirs(OUT, exist_ok=True)                            # create output/ if it does not exist yet (no error if it does)

# Numeric codebook value -> human-readable actor category (used for labelling only).
ACTOR_TYPE = {1: "NGO/Foundation/Trust", 2: "Research/Academic",
              3: "Umbrella/Network", 4: "Government",
              5: "Conservancy/Protected Area", 6: "Consultancy/Private/Other"}
# The 14 coded framing / style variables. In analysis_base.csv each is a 0/1 column
# named "f_<Name>" (e.g. f_Empathy). Averaging a 0/1 column over an org's posts gives
# the PREVALENCE of that frame for that org (e.g. 0.30 = 30% of its posts use empathy).
FRAMES = ["Empathy", "Threat", "Efficacy", "Collective_ID", "Normative", "Moral",
          "Economic", "Scientific", "Youth_Addressed", "Interactivity",
          "Youth_Style", "Protagonist", "Onscreen_Text", "Data_Visual"]

# Normalised Shannon entropy of a count vector, returned in [0, 1].
# Feeds "genre_entropy": how EVENLY an org spreads its posts across content genres.
# 0 = all posts in one genre (a single-topic account); 1 = posts split perfectly
# evenly across its genres (a generalist). Dividing by log2(k), where k is the number
# of non-empty genres, rescales the raw entropy so accounts with different numbers of
# genres are still comparable on the same 0-1 scale.
def norm_entropy(counts):
    p = np.asarray([c for c in counts if c > 0], float)    # keep only non-zero genre counts
    if p.size < 2:
        return 0.0                                         # 0 or 1 genre used -> no spread -> entropy 0
    p = p / p.sum()                                        # turn counts into a probability distribution
    return float(-(p * np.log2(p)).sum() / np.log2(p.size))  # Shannon entropy / log2(k) = normalised to 0-1

# ---- load ---------------------------------------------------------------------
# analysis_base.csv is the master coded corpus. sep=";" and utf-8-sig because it was
# exported from a European-locale spreadsheet (semicolon delimiter, UTF-8 BOM).
df = pd.read_csv(str(_BASE), sep=";", encoding="utf-8-sig")
d = df[df["coded"] == 1].copy()                            # keep only posts that passed coding (drop the rest); .copy() avoids a chained-assignment warning
# Force the metric + frame columns to numeric. errors="coerce" turns any stray
# non-numeric cell into NaN instead of crashing, so a single bad row cannot stop the run.
for c in ["Followers", "eng_rate", "clip_align", "Likes", "Comments"] + ["f_" + f for f in FRAMES]:
    d[c] = pd.to_numeric(d[c], errors="coerce")
d["Posting_Date"] = pd.to_datetime(d["Posting_Date"], errors="coerce")   # parse the post date so we can compute posting span / cadence

# Phase A genre labels: sampling_frame.csv maps each Post_ID to a functional_genre
# (e.g. "awareness", "fundraising"). Build a lookup dict, then attach it to each post.
sf = pd.read_csv(os.path.join(OUT, "sampling_frame.csv"))
genre = sf.set_index("Post_ID")["functional_genre"].to_dict()
d["functional_genre"] = d["Post_ID"].map(genre).fillna("unlabelled")   # posts with no label become "unlabelled" rather than NaN

# Account-level snapshot of the 19 organizations, taken 3 July 2026: follower and
# following counts, lifetime post count, highlight count, business and verified flags.
# Keyed by lower-cased username so each org's row can be looked up by its handle.
PROF = {p["username"].lower(): p for p in json.load(open(ORG_PROFILES, encoding="utf-8"))}

# ---- aggregate per org --------------------------------------------------------
# Group the post-level corpus by organisation (identity = name + handle + actor type),
# then compute one summary row per org. groupby yields (key, sub-frame g) pairs.
rows = []
for (name, handle, atype), g in d.groupby(["Actor_Name", "Handle", "Actor_Type"]):
    handle = str(handle).lower()                           # normalise handle for the profile-dict lookup
    pr = PROF.get(handle, {})                              # this org's Apify profile record (empty dict if not matched)
    n = len(g)                                             # number of coded posts for this org
    fmt = g["Post_Format"].value_counts(normalize=True)    # share of Video / Carousel / Image posts (proportions, sum to 1)
    # Posting span in days: last post date minus first. Guarded so an org with no
    # valid dates yields NaN instead of throwing.
    span_days = (g["Posting_Date"].max() - g["Posting_Date"].min()).days if g["Posting_Date"].notna().any() else np.nan
    active_days = g["Posting_Date"].dt.date.nunique()      # count of DISTINCT calendar days the org posted on
    gc = g["functional_genre"].value_counts()              # how many posts fall in each genre (for entropy + dominant genre)
    likes = g["Likes"].fillna(0)                           # treat missing engagement as 0 for the medians below
    comments = g["Comments"].fillna(0)
    row = {
        "Actor_Name": name, "Handle": handle,
        "Actor_Type": int(atype), "Actor_Type_label": ACTOR_TYPE.get(int(atype), "?"),   # numeric code + readable label
        "n_posts": n,
        # profile (account-level structure) - pulled straight from the Apify snapshot
        "followers": pr.get("followersCount"),
        "follows": pr.get("followsCount"),
        "posts_total": pr.get("postsCount"),               # lifetime post count (not just the sampled window)
        "verified": int(bool(pr.get("verified"))),         # blue-tick as 0/1
        "is_business": int(bool(pr.get("isBusinessAccount"))),
        "highlight_reels": pr.get("highlightReelCount"),   # number of Story Highlights = curated-account signal
        # follower/following ratio: a rough "authority" proxy (many followers, few follows).
        # Guarded: if followsCount is 0/missing, return NaN instead of dividing by zero.
        "follow_ratio": (pr.get("followersCount") / pr.get("followsCount"))
                        if pr.get("followsCount") else np.nan,
        # format mix - what share of the org's posts are each medium (0 if never used)
        "pct_video": float(fmt.get("Video", 0.0)),
        "pct_carousel": float(fmt.get("Carousel", 0.0)),
        "pct_image": float(fmt.get("Image", 0.0)),
        # caption style (medians / rates) - median is robust to the odd very long caption
        "cap_chars_med": float(g["cap_chars"].median()),   # typical caption length in characters
        "cap_words_med": float(g["cap_words"].median()),   # typical caption length in words
        "hashtags_med": float(g["n_hashtags"].median()),
        "emojis_med": float(g["n_emojis"].median()),
        "mentions_med": float(g["n_mentions"].median()),
        "link_rate": float(g["has_link"].mean()),          # share of posts that include a link (0/1 mean = proportion)
        "question_rate": float(g["has_question"].mean()),  # share of posts that ask a question (dialogue cue)
        # visual style (rates) - mean of the 0/1 LLM visual flags = prevalence
        "onscreen_text_rate": float(g["f_Onscreen_Text"].mean()),   # how often the org burns text onto the image/video
        "protagonist_rate": float(g["f_Protagonist"].mean()),       # how often a human/animal protagonist is featured
        "data_visual_rate": float(g["f_Data_Visual"].mean()),       # how often charts/infographics appear
        "youth_style_rate": float(g["f_Youth_Style"].mean()),       # how often a youth-oriented visual style is used
        "clip_align_med": float(g["clip_align"].median()),          # median image-caption semantic alignment (CLIP)
        # engagement (public metrics only; rate = likes / followers)
        "eng_rate_med": float(g["eng_rate"].median()),     # median engagement rate (already computed upstream as likes/followers)
        "likes_med": float(likes.median()),
        "comments_med": float(comments.median()),
        # comment-to-like ratio: how "conversational" the audience is. +1 in the
        # denominator avoids division by zero on posts with no likes.
        "comment_like_ratio_med": float((comments / (likes + 1)).median()),
        # content portfolio - is the org single-topic or a generalist?
        "genre_entropy": norm_entropy(gc.values),          # 0 = one topic, 1 = evenly spread across genres
        "n_genres": int((gc > 0).sum()),                   # how many distinct genres the org touched
        "dominant_genre": gc.index[0] if len(gc) else "",  # its single most frequent genre (value_counts is sorted desc)
        "dominant_genre_share": float(gc.iloc[0] / n) if len(gc) else np.nan,  # what fraction of posts sit in that top genre
        # cadence - how regularly the org posts
        "span_days": span_days,
        "active_days": int(active_days),
        "posts_per_week": float(n / (span_days / 7)) if span_days and span_days > 0 else np.nan,  # posting frequency; guarded against zero/short spans
    }
    # frame prevalence per org: mean of each 0/1 frame column = share of this org's
    # posts using that frame. These 14 columns are the core of the style typology.
    for f in FRAMES:
        row["frame_" + f] = float(g["f_" + f].mean())
    rows.append(row)

# Assemble the 19-row matrix and order it by corpus footprint (most-posting org first).
om = pd.DataFrame(rows).sort_values("n_posts", ascending=False)
om.to_csv(os.path.join(OUT, "org_feature_matrix.csv"), index=False)   # this CSV is the input to the R clustering script

# ---- audit --------------------------------------------------------------------
# Console summary so the run is self-checking: matrix shape, how many orgs matched a
# profile snapshot, and a readable preview of the headline features.
print(f"c1_org_features.py: {len(om)} orgs x {om.shape[1]} features -> output/org_feature_matrix.csv")
print("\nProfile join: {}/{} orgs matched a profile snapshot".format(
    int(om["followers"].notna().sum()), len(om)))          # notna().sum() = how many orgs got a follower count from Apify
show = om[["Actor_Name", "Actor_Type_label", "n_posts", "followers",
           "cap_chars_med", "pct_video", "onscreen_text_rate", "eng_rate_med",
           "genre_entropy", "dominant_genre"]].copy()       # pick a compact set of columns for the preview
show["eng_rate_med"] = (100 * show["eng_rate_med"]).round(2)   # show engagement as a percentage
show["followers"] = show["followers"].astype("Int64")      # nullable integer so a missing follower count prints blank, not 1234.0
# option_context temporarily lifts pandas' row/width caps so the whole preview prints untruncated.
with pd.option_context("display.max_rows", None, "display.width", 200):
    print("\n" + show.to_string(index=False))
