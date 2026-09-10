#!/usr/bin/env python3
"""
Where everything lives in this package.

The scripts in python/ were written inside the working tree of the thesis, where
the corpus, the NLP outputs and the feature tables sat in sibling folders with
names like "2. Data Collection (Apify)". In the reproduction package the same
files live under data/. This module is the one place that knows the mapping, so
no script has to carry a path that only resolves on the author's machine.

Import it and use the constants:

    from _paths import RAW_CORPUS, ANALYSIS_BASE, FEATURES

Every constant is an absolute pathlib.Path, so a script runs from any working
directory.
"""

from pathlib import Path

# python/_paths.py -> python/ -> package root
PKG = Path(__file__).resolve().parent.parent

DATA      = PKG / "data"
RAW       = DATA / "raw"
PROCESSED = DATA / "processed"
STUDY1    = DATA / "study1"
FEATURES  = STUDY1 / "features"
OUTPUT    = PKG / "output"
TABLES    = OUTPUT / "tables"

# Study 1 corpus and the two passes over it
RAW_CORPUS    = RAW / "Study1" / "Study1_Posts_RAW_20260520.csv"
ORG_PROFILES  = RAW / "Study1" / "Study1_Org_Profiles_2026-07-03.json"
LLM_CODES     = STUDY1 / "Study1_LLM_multimodal_920.csv"
LLM_REVIEWED  = STUDY1 / "Study1_LLM_multimodal_reviewed.csv"
BLIND_CODED   = STUDY1 / "Study1_BlindCoded_184.csv"

# the master per-post table every feature script joins onto
ANALYSIS_BASE = STUDY1 / "analysis_base.csv"
SAMPLE_150    = STUDY1 / "sample_150.csv"
COMMENTS_TEXT = STUDY1 / "comments_text.csv"

# Study 2 second coding
SECOND_CODING = DATA / "study2" / "second_coding"


def feature(name: str) -> Path:
    """Path to a feature table, with or without the .csv suffix."""
    return FEATURES / (name if name.endswith(".csv") else name + ".csv")


# Inputs that are NOT part of the package, named here so a script can say so
# instead of failing on a path nobody can resolve.
MISSING = {
    "post_images": "The scraped post images. Not shipped: copyright, and 520 files. "
                   "The CLIP scripts that need them are record-only.",
    "apify_json":  "The raw Apify export. Not shipped; the RAW corpus CSV it "
                   "produced is included as RAW_CORPUS.",
}
