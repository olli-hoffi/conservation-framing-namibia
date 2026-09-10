#!/usr/bin/env python3
"""
Phase C - Phase-B post-processing (data-generation). Turns the single-coder Phase B
LLM output into tidy analysis features for the four Phase-B-dependent analyses.
READ-ONLY on the Phase B files (llm_stage1.csv, llm_stage1b_comments.csv); writes
new derived tables only.

Produces:
  - consolidated functional_genre (132 raw labels -> 11 synthesis categories, keyword map)
  - appeal_* binary indicators (6 atomic appeals; appeal_type is multi-coded)
  - target-audience indicators and post counts (labels may co-occur)
  - cta_present + visual-quality ordinals (aesthetic / effort / human presence / on-image text)
  - image-vs-caption species congruence class (do orgs show what they name)
  - per-comment stance table (support / criticism / question / off-topic / spam)

Reads : llm_stage1.csv, llm_stage1b_comments.csv (Phase B, read-only)
Writes: output/llm_post_features.csv, output/llm_target_audience_counts.csv,
        output/llm_comment_stance.csv
Run   : "../4. NLP Pipeline/.venv/bin/python" b2_taxonomy_features.py
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
import os, re
import numpy as np
import pandas as pd

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
from _paths import ANALYSIS_BASE as _BASE, RAW as _RAW, FEATURES as _FEAT, STUDY1 as _STUDY1

OUT = str(_FEAT)
os.makedirs(OUT, exist_ok=True)

# Phase B outputs, read-only: s1 = one row per post (Stage 1), s1b = one row per comment (Stage 1b).
s1 = pd.read_csv(str(_STUDY1 / "llm_stage1.csv"))
s1b = pd.read_csv(str(_STUDY1 / "llm_stage1b_comments.csv"))

# ---- 1. consolidate functional_genre -> 11 synthesis categories ---------------
# ordered keyword rules (first match wins); a heuristic consolidation of the
# single-coder free-text labels onto the Stage-2 synthesis taxonomy.
# Each tuple is (category name, [substrings]); map_genre scans the raw label lowercased
# and returns the FIRST category whose keyword list matches, so order encodes priority.
GENRE_RULES = [
    ("Rescue & animal case", ["rescue", "release", "orphan", "rehabilit", "ambassador",
                              "injured", "case update", "case narrative", "stranded", "adopt"]),
    ("Awareness-day & cause", ["awareness", "commemorat", "observance", "world ", "earth hour",
                               "earth day", "independence", "biodiversity day", "wetlands day",
                               "wildlife day", "penguin day", "pangolin day", "-day", "cause "]),
    ("Broadcast & media", ["tv ", "episode", "broadcast", "series", "trailer", "teaser",
                           "media promotion", "media launch", "documentary promo", "film", "podcast"]),
    ("Recruitment/fundraising/participation", ["vacancy", "job ", "recruit", "volunteer", "fundrais",
                                               "donat", "sponsor", "competition", "survey", "rsvp",
                                               "invitation", "bursary", "quiz", "entry", "apply",
                                               "participation", "opinion", "poll", "giveaway"]),
    ("Recognition/tribute/gratitude", ["thank", "gratitude", "recognition", "tribute", "congratulat",
                                       "honour", "honor", "memorial", "farewell", "appreciation post"]),
    ("Institutional announcement", ["institutional", "meeting report", "appointment", "agreement",
                                    "policy", "mou", "personnel", "service disruption", "statement",
                                    "official announcement", "corporate"]),
    ("Educational explainer", ["education", "explainer", "fun-fact", "fun fact", "fact ", "infographic",
                               "method", "research finding", "research-finding", "guide", "tip",
                               "did you know", "how-to", "how to", "species spotlight", "data "]),
    ("Tourism & lodge", ["lodge", "tourism", "safari", "destination", "rebrand", "amenity",
                         "guest", "accommodation", "hospitality", "experience promotion",
                         "atmosphere", "stay ", "reserve promotion", "resort"]),
    ("Programme/fieldwork/impact", ["programme", "program", "fieldwork", "field ", "impact",
                                    "operation", "conservation work", "event", "training",
                                    "conference", "workshop", "mission", "monitoring", "patrol",
                                    "project", "initiative", "activity", "symposium", "report"]),
    ("Wildlife showcase & feel-good", ["wildlife showcase", "feel-good", "feel good", "beauty",
                                       "mood", "playful", "cute", "portrait", "sighting",
                                       "appreciation", "showcase", "greeting card"]),
    ("Seasonal greeting", ["seasonal", "holiday", "weekend", "april fool", "festive", "greeting",
                           "new year", "christmas", "valentine", "gag"]),
]
def map_genre(raw):
    """Return the first GENRE_RULES category whose keywords appear in the raw label,
    else 'Other/unmapped'. First-match-wins, so rule order is the tie-break."""
    r = str(raw).lower()
    for cat, kws in GENRE_RULES:
        if any(k in r for k in kws):
            return cat
    return "Other/unmapped"
# Collapse ~132 free-text genre labels onto the 11 consolidated synthesis categories.
s1["genre_consolidated"] = s1["functional_genre"].map(map_genre)

# ---- 2. appeal_type -> binary indicators --------------------------------------
# appeal_type is MULTI-coded (a post can carry several appeals, delimited by |,;/).
# Split it into a set, then make one 0/1 indicator column per atomic appeal below.
APPEALS = ["emotional", "identity", "informational", "aesthetic", "social-normative", "economic"]
def split_appeals(x):
    """Split a multi-appeal string into a set of atomic appeal tokens (normalising the
    'social normative' spelling to the hyphenated 'social-normative')."""
    toks = re.split(r"[|,;/]", str(x).lower())
    toks = [t.strip().replace("social normative", "social-normative") for t in toks]
    return set(t for t in toks if t)
appset = s1["appeal_type"].map(split_appeals)      # one set of appeals per post
for a in APPEALS:
    # appeal_<name> = 1 if that appeal is present for the post, else 0 (dummy-code each appeal).
    s1["appeal_" + a.replace("-", "_")] = appset.map(lambda s: int(a in s))
s1["n_appeals"] = appset.map(len)                  # how many appeals the post combines

# ---- 3. Target audience -> indicators and counts ------------------------------
# target_audience carries one or more schema labels separated by commas, pipes or
# semicolons. Count each label once per post even if a malformed response repeats it.
AUDIENCES = ["general-public", "supporters", "tourists", "peers/researchers",
             "donors", "youth", "policymakers", "local-community"]
def split_audiences(x):
    """Return the unique normalized audience labels attached to one post."""
    toks = re.split(r"[|,;]", str(x).lower())
    return set(t.strip() for t in toks if t.strip() and t.strip() != "nan")

audset = s1["target_audience"].map(split_audiences)
audience_cols = []
for audience in AUDIENCES:
    col = "audience_" + audience.replace("/", "_").replace("-", "_")
    audience_cols.append(col)
    s1[col] = audset.map(lambda labels: int(audience in labels))

audience_counts = pd.DataFrame({
    "audience_label": AUDIENCES,
    "n_posts": [int(s1[col].sum()) for col in audience_cols],
})
audience_counts["n_subsample_posts"] = len(s1)
audience_counts["pct_posts"] = (100 * audience_counts["n_posts"] / len(s1)).round(1)
audience_counts.to_csv(os.path.join(OUT, "llm_target_audience_counts.csv"), index=False)

# ---- 4. CTA + visual-quality ordinals -----------------------------------------
# cta_present = 1 unless the coded call-to-action is literally "none" (a binary has-CTA flag).
s1["cta_present"] = (s1["actual_cta"].astype(str).str.strip().str.lower() != "none").astype(int)
# Turn the categorical visual-quality codes into ORDINALS (rank scores) so they can be
# averaged/correlated. Note amateur and screenshot-graphic BOTH map to 1 = lowest tier.
AES = {"amateur": 1, "screenshot-graphic": 1, "competent": 2, "professional": 3}
EFF = {"low": 1, "medium": 2, "high": 3}
HUM = {"none": 0, "one-person": 1, "group": 2, "crowd": 3}   # count-of-people ordinal
TXT = {"none": 0, "minimal": 1, "moderate": 2, "text-dominant": 3}   # amount of on-image text
s1["vq_aesthetic_ord"] = s1["vq_aesthetic"].str.strip().str.lower().map(AES)
s1["vq_effort_ord"] = s1["vq_production_effort"].str.strip().str.lower().map(EFF)
s1["vq_human_ord"] = s1["vq_human_presence"].str.strip().str.lower().map(HUM)
s1["vq_text_ord"] = s1["vq_on_image_text_amount"].str.strip().str.lower().map(TXT)
s1["has_people"] = (s1["vq_human_ord"] > 0).astype("Int64")   # any human present? (nullable Int64 keeps NA as NA)
# Colour-mood is free text -> derive 4 non-exclusive mood flags by keyword (a post can be both).
cm = s1["vq_colour_mood"].fillna("").str.lower()
s1["mood_warm"] = cm.str.contains(r"warm|golden|amber|ochre|earth").astype(int)
s1["mood_cool"] = cm.str.contains(r"cool|blue|teal|grey|gray|clinical").astype(int)
s1["mood_muted"] = cm.str.contains(r"muted|desaturat|washed|faded|sober|flat").astype(int)
s1["mood_vibrant"] = cm.str.contains(r"vibrant|bright|saturated|vivid|bold").astype(int)

# ---- 5. image-vs-caption species congruence -----------------------------------
ANIMALS = ["elephant","rhino","cheetah","lion","leopard","giraffe","pangolin","hyena","zebra",
           "seal","penguin","whale","dolphin","turtle","oryx","gemsbok","springbok","kudu",
           "buffalo","hippo","wild dog","painted dog","vulture","eagle","flamingo","ostrich",
           "crocodile","snake","python","meerkat","baboon","jackal","antelope","fish","shark",
           "wildebeest","warthog","aardvark","caracal","serval","bird","insect","bee","frog"]
def animal_set(x):
    """Return the set of ANIMALS keywords found in a free-text species field (empty set for
    blank/NaN). Used to compare what the caption NAMES vs. what the image SHOWS."""
    t = str(x).lower()
    if t in ("nan", ""):
        return set()
    return set(a for a in ANIMALS if a in t)
cap_sp = s1["species_mentioned"].map(animal_set)   # species named in the caption
vis_sp = s1["vq_visual_species"].map(animal_set)   # species visible in the image
def congruence(cs, vs):
    """Classify caption-vs-image species overlap: neither has any, both overlap (match),
    both present but disjoint (mismatch), named-only, or shown-only. A 'show vs tell' check."""
    if not cs and not vs:
        return "neither"
    if cs and vs:
        return "match" if cs & vs else "mismatch"   # cs & vs = set intersection; non-empty = they agree
    if cs and not vs:
        return "names_not_shows"
    return "shows_not_names"
s1["species_congruence"] = [congruence(c, v) for c, v in zip(cap_sp, vis_sp)]
s1["caption_species_set"] = cap_sp.map(lambda s: ";".join(sorted(s)))   # flatten the set for the CSV
s1["visual_species_set"] = vis_sp.map(lambda s: ";".join(sorted(s)))

# ---- write per-post features --------------------------------------------------
# The analysis-ready per-post table: consolidated genre + all derived features (appeal
# dummies, CTA flag, visual-quality ordinals, mood flags, species congruence).
keep = ["post_id", "genre_consolidated", "functional_genre", "narrative_structure",
        "actual_cta", "cta_present", "target_audience", "register_tone", "image_role",
        "image_caption_relation", "n_appeals"] + \
       ["appeal_" + a.replace("-", "_") for a in APPEALS] + audience_cols + \
       ["vq_aesthetic_ord", "vq_effort_ord", "vq_human_ord", "vq_text_ord", "has_people",
        "mood_warm", "mood_cool", "mood_muted", "mood_vibrant",
        "species_congruence", "caption_species_set", "visual_species_set", "missed_visual_flag"]
s1[keep].to_csv(os.path.join(OUT, "llm_post_features.csv"), index=False)

# ---- per-comment stance -------------------------------------------------------
# Stance table passes through unchanged (already one row per comment from Phase B).
s1b.to_csv(os.path.join(OUT, "llm_comment_stance.csv"), index=False)

# ---- audit --------------------------------------------------------------------
# Console summary so the feature build can be sanity-checked at a glance: how many posts fell
# in each consolidated genre (and how many stayed unmapped), appeal prevalence, species
# congruence mix, average visual quality, and the comment-stance distribution.
print(f"b2_taxonomy_features.py: {len(s1)} posts -> llm_post_features.csv | "
      f"{len(s1b)} comments over {s1b.post_id.nunique()} posts -> llm_comment_stance.csv")
print("\nConsolidated functional_genre (heuristic map of 132 raw labels):")
gc = s1["genre_consolidated"].value_counts()
print(gc.to_string())
print(f"  unmapped: {int(gc.get('Other/unmapped', 0))}/{len(s1)}")
print("\nAppeal prevalence (multi-coded, % of 150 posts):")
for a in APPEALS:
    print(f"  {a:18s} {100*s1['appeal_'+a.replace('-','_')].mean():.0f}%")
print("\nNamed audiences (labels may co-occur):")
print(audience_counts[["audience_label", "n_posts", "pct_posts"]].to_string(index=False))
print("\nSpecies congruence (show-vs-tell):")
print(s1["species_congruence"].value_counts().to_string())
print("\nVisual quality:")
print(f"  aesthetic (1-3) mean {s1.vq_aesthetic_ord.mean():.2f} | effort mean {s1.vq_effort_ord.mean():.2f} "
      f"| has_people {100*s1.has_people.mean():.0f}% | text-heavy(>=2) {100*(s1.vq_text_ord>=2).mean():.0f}%")
print("\nComment stance distribution:")
print(s1b.stance.value_counts().to_string())
print(f"  criticism rate: {100*(s1b.stance=='criticism').mean():.1f}% | engages_frame: {100*s1b.engages_frame.mean():.0f}%")
