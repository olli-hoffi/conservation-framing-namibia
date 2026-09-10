#!/usr/bin/env python3
"""
Phase A.4 (frame only) - build the sampling frame and PRINT a proposed allocation.

Does NOT draw the final sample. Produces the strata + coverage statistics needed to
approve the stratification scheme before drawing sample_150.csv. Token-free.

Builds, per coded post:
  functional_genre   - coarse communicative-function stratum (BERTopic-cluster -> function map)
  new_genre flags    - keyword-detected genres the a-priori codebook has no slot for
                       (commemoration_day / live_series / hiring / pure_info)
  pca1..3            - per-post scores on the 3 latent frame dimensions
                       (emotional-character / institutional-advocacy / scientific);
                       computed here ONLY as a sampling axis (reported PCA stays in R)
  comment_avail      - scraped comments available for the stance layer
  blindspot          - membership in the T2 no-frame set (output/blindspots.csv)

Run with the NLP venv:
  "../4. NLP Pipeline/.venv/bin/python" a6_sampling_frame.py
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
# Status: runs offline against the shipped data.
#
import os
import re
import numpy as np
import pandas as pd

# --- paths: every location comes from python/_paths.py, so the script runs from
# anywhere inside the package and never depends on where it sits ---
import sys as _sys
_here = os.path.dirname(os.path.abspath(__file__))
# Walk up until _paths.py appears, so moving a script between subfolders does not
# break the import and no fixed number of ".." has to be kept in sync.
while _here != os.path.dirname(_here) and not os.path.exists(os.path.join(_here, "_paths.py")):
    _here = os.path.dirname(_here)
_sys.path.insert(0, _here)
from _paths import ANALYSIS_BASE, RAW_CORPUS, FEATURES
OUT_DIR = str(FEATURES)

# The 14 frame indicators (f_ prefix = frame) produced by manual coding / the NLP pass.
# These are the columns the per-post PCA below runs on to build the 3-dimensional
# "frame space" used ONLY as a sampling axis. It enters no reported result. It only
# defines the terciles the n = 150 subsample is stratified on (Table B1).
FRAMES = [
    "f_Empathy", "f_Threat", "f_Efficacy", "f_Collective_ID", "f_Normative",
    "f_Moral", "f_Economic", "f_Scientific", "f_Youth_Addressed",
    "f_Interactivity", "f_Youth_Style", "f_Protagonist", "f_Onscreen_Text",
    "f_Data_Visual",
]

# BERTopic cluster -> coarse functional-genre stratum. Dominant-function mapping;
# the per-post TRUE genre is what Phase B derives inductively. -1/-99 -> outliers.
GENRE_MAP = {
    "rescue_intervention": [3, 18, 26],
    "wildlife_series_broadcast": [21],
    "species_spotlight_facts": [5, 31],
    "research_dispatch": [12, 13, 17, 19, 25, 27, 32],
    "monitoring_conservation_tech": [4, 15],
    "ecotourism_aesthetic": [1, 2],
    "weekly_ritual_filler": [8],
    "commemoration_awareness": [20],
    "policy_governance": [6, 14, 24, 29, 30],
    "coexistence_community_livelihoods": [0, 9, 10, 33],
    "education_youth": [7, 16, 23],
    "org_update_identity": [11, 22, 28],
}
# Invert GENRE_MAP into a flat {cluster_id: genre} lookup so a post's BERTopic cluster
# number maps straight to its coarse functional genre. The nested comprehension walks
# every (genre, [cluster ids]) pair and emits one entry per cluster id.
CLUSTER2GENRE = {c: g for g, cs in GENRE_MAP.items() for c in cs}

# Keyword detectors for NEW genres the a-priori codebook has no slot for. Each is a
# compiled case-insensitive (re.I) regex run over the caption; a match flags the post as
# belonging to that emergent genre, independent of its BERTopic cluster. These flags feed
# the "coverage floor" logic in the draw step (a7) so rare genres are not missed.
# keyword detectors for NEW genres (per-post, independent of cluster)
KW = {
    # world/earth/international days, commemorations, awareness days & months
    "commemoration_day": re.compile(
        r"\b(?:world .{0,20}day|earth day|earth hour|international day of|"
        r"\w+ day 20\d\d|independence|commemorat|arbor day|awareness (?:day|month))\b", re.I),
    # broadcast/live formats: episodes, premieres, tune-in and live-session cues
    "live_series": re.compile(
        r"\b(?:episode|don.?t miss|tune in|premiere|live (?:session|hour|q ?& ?a|stream)|"
        r"watch .{0,30}(?:on|channel)|new episode|this week on|groen)\b", re.I),
    # recruitment: vacancies, hiring, job/volunteer calls-to-apply
    "hiring": re.compile(
        r"\b(?:vacancy|vacancies|career opportunity|we are hiring|now hiring|"
        r"seeking a|job opportunity|apply (?:now|by|before)|recruit|is looking for a)\b", re.I),
    # pure information: fun-facts, explainers, "did you know", introductions
    "pure_info": re.compile(
        r"\b(?:did you know|fun fact|fast facts|fact of the|introducing|meet the|"
        r"what is |all about|here.?s how|things you)\b", re.I),
}


def main():
    # Load the coded corpus (semicolon-separated) and keep only rows actually coded
    # (coded == 1). N is the size of the coded set this sampling frame describes.
    base = pd.read_csv(ANALYSIS_BASE, sep=";")
    base = base[base["coded"] == 1].copy()
    N = len(base)

    # caption text for keyword flags
    # Read the NLP export purely for its caption column. comment="#" skips '#'-prefixed
    # header lines; utf-8-sig strips the byte-order mark some exports prepend.
    nlp = pd.read_csv(RAW_CORPUS, sep=";", comment="#", encoding="utf-8-sig")
    # Pick the caption column defensively: prefer "Caption_Text", else fall back to the
    # first column whose name contains "aption" (guards against header-name drift).
    capcol = "Caption_Text" if "Caption_Text" in nlp.columns else nlp.columns[
        [i for i, c in enumerate(nlp.columns) if "aption" in c][0]]
    # Index captions by Post_ID, then map them onto base; posts with no caption become "".
    caps = nlp.set_index("Post_ID")[capcol].fillna("").astype(str)
    base["caption"] = base["Post_ID"].map(caps).fillna("")

    # functional genre
    # Map each post's BERTopic cluster id to its coarse functional-genre stratum...
    base["functional_genre"] = base["BERTopic"].map(CLUSTER2GENRE)
    # ...then fold the two outlier cluster ids (-1 = noise, -99 = unassigned) into one bucket.
    base.loc[base["BERTopic"].isin([-1, -99]), "functional_genre"] = "outlier_atypical"

    # new-genre keyword flags
    # One boolean column per NEW genre: True where the caption text matches that regex.
    for name, rx in KW.items():
        base[name] = base["caption"].str.contains(rx)

    # blindspot membership
    # Tag posts in the T2 "no-frame" overlay pool (their ids are listed in blindspots.csv).
    bs = pd.read_csv(os.path.join(OUT_DIR, "blindspots.csv"))
    base["blindspot"] = base["Post_ID"].isin(set(bs["Post_ID"]))

    # per-post PCA on 14 frames (sampling axis only). 38 posts without a usable
    # image have NaN visual frames -> treat absent (fill 0) for the sampling axis.
    X = base[FRAMES].fillna(0).to_numpy(dtype=float)
    # z-standardise each frame column (mean 0, SD 1) so a high-variance frame does not
    # dominate the PCA on scale alone; the + 1e-9 avoids divide-by-zero on a constant
    # (zero-variance) column.
    Xs = (X - X.mean(0)) / (X.std(0) + 1e-9)
    from sklearn.decomposition import PCA
    # Compress the 14 correlated frames into their 3 leading components (the "frame space"
    # the draw will spread the sample over). random_state=42 fixes the solver so the
    # component scores - and the terciles below - come out identical on every re-run.
    pcs = PCA(n_components=3, random_state=42).fit_transform(Xs)
    for i in range(3):
        base[f"pca{i+1}"] = pcs[:, i]                        # continuous score on component i+1
        # Split each component into lo/mid/hi terciles (equal-frequency bins, ~1/3 of posts
        # each) so the draw can stratify by frame position. duplicates="drop" tolerates tied
        # bin edges when many posts share the same score.
        base[f"pca{i+1}_bin"] = pd.qcut(pcs[:, i], 3, labels=["lo", "mid", "hi"],
                                        duplicates="drop")

    # comment availability
    # Stance-eligible = at least 3 scraped comments (enough to code a dominant stance in Phase B).
    base["comment_avail"] = base["n_comments_scraped"] >= 3

    # ---- report ----
    print(f"# Sampling frame  (N={N} coded posts)\n")
    print("## Functional-genre strata (primary stratification variable)")
    # One row per functional genre: corpus count, the post-format mix, overlay-pool counts,
    # median engagement, and org spread - the numbers the draw needs in order to balance.
    g = base.groupby("functional_genre", dropna=False)
    tab = pd.DataFrame({
        "n": g.size(),                                      # posts in this genre
        "n_video": g.apply(lambda d: (d["Post_Format"] == "Video").sum(), include_groups=False),
        "n_carousel": g.apply(lambda d: (d["Post_Format"] == "Carousel").sum(), include_groups=False),
        "n_image": g.apply(lambda d: (d["Post_Format"] == "Image").sum(), include_groups=False),
        "n_blindspot": g["blindspot"].sum(),                # how many are T2 no-frame posts
        "n_with_comments": g["comment_avail"].sum(),        # how many are stance-eligible
        "med_eng_rate%": (g["eng_rate"].median() * 100).round(2),   # median engagement, as a percentage
        "n_orgs": g["Actor_Name"].nunique(),                # how many distinct orgs post in this genre
    }).sort_values("n", ascending=False)

    # compromise allocation: proportional-to-sqrt, floor 6 (outliers floor 8), target ~112 core
    # sqrt(n) weighting sits between equal and proportional allocation: it compresses the
    # dominant genres and lifts the rare ones, so small strata are not starved of draws.
    counts = tab["n"].astype(float)
    raw = np.sqrt(counts)
    core_target = 112
    # Scale sqrt-weights to sum to ~112, round to whole posts, then impose a floor of 6 per genre.
    alloc = np.maximum(np.round(raw / raw.sum() * core_target), 6).astype(int)
    alloc.loc["outlier_atypical"] = max(int(alloc.get("outlier_atypical", 0)), 8)   # atypical tail gets floor 8
    alloc = alloc.clip(upper=counts.astype(int))            # never allocate more than the genre actually has
    tab["ALLOC"] = alloc
    print(tab.to_string())
    print(f"\n  core allocation sums to {int(tab['ALLOC'].sum())} "
          f"(before overlays); genres = {len(tab)}")

    # For each keyword genre, report how many posts match and how many of those also
    # carry comments; this sizes the coverage floors the draw enforces on top of core.
    print("\n## NEW-genre keyword coverage (guarantee floors on top of core)")
    for name in KW:
        n = int(base[name].sum())                          # posts matching this keyword genre
        withc = int(base[base[name]]["comment_avail"].sum())   # ...of which are also stance-eligible
        print(f"  {name:20s} matches {n:4d}  (with comments {withc})")

    print("\n## Overlay pools")
    print(f"  blindspot (T2 no-frame)     : {int(base['blindspot'].sum())}")
    print(f"  outlier BERTopic -1 / -99   : {int((base['BERTopic']==-1).sum())} / "
          f"{int((base['BERTopic']==-99).sum())}")
    print(f"  posts with >=3 comments     : {int(base['comment_avail'].sum())}")
    # The 20 most-commented posts preview the high-discussion stance layer the draw builds.
    top_comments = base.nlargest(20, 'n_comments_scraped')[['Post_ID', 'n_comments_scraped', 'functional_genre']]
    print(f"  viral (top comment counts)  : max={int(base['n_comments_scraped'].max())}, "
          f"top-20 range {int(top_comments['n_comments_scraped'].min())}-{int(top_comments['n_comments_scraped'].max())}")

    print("\n## PCA dimension bins (frame-space stratification axis)")
    print("  pca1 = emotional-character | pca2 = institutional-advocacy | pca3 = scientific")
    print("  each split into lo/mid/high terciles (~1/3 each by construction)")

    # visual-genre coverage
    vc = pd.read_csv(os.path.join(OUT_DIR, "visual_clusters.csv"))
    vg = vc["visual_zeroshot"].value_counts()
    print("\n## Visual-genre coverage pool (CLIP zero-shot)")
    print("  " + ", ".join(f"{k} {v}" for k, v in vg.items()))

    # save the enriched frame for the draw step
    # Carry only the columns the draw (a7) needs: ids, strata (genre, format, PCA bins),
    # the overlay flags and the new-genre keyword columns. This CSV is the draw's input.
    keep = ["Post_ID", "Actor_Name", "Post_Format", "BERTopic", "functional_genre",
            "blindspot", "comment_avail", "n_comments_scraped", "eng_rate",
            "pca1", "pca2", "pca3", "pca1_bin", "pca2_bin", "pca3_bin"] + list(KW)
    base[keep].to_csv(os.path.join(OUT_DIR, "sampling_frame.csv"), index=False)
    print(f"\nWrote enriched frame -> {os.path.join(OUT_DIR, 'sampling_frame.csv')} (NOT the sample; draw is gated)")


if __name__ == "__main__":
    main()
