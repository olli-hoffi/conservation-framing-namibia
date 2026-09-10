#!/usr/bin/env python3
"""
Phase A.2 - Codebook blind-spots.

A "blind-spot" post is one the a-priori codebook cannot describe: no narrative
structure (Story = None), no ask (CTA = None), and no persuasive frame firing.
Three nested tiers are reported because the 14 frames mix persuasion frames with
purely descriptive visual frames (Protagonist / Onscreen_Text / Data_Visual):

  T1 STRICT   Story=None AND CTA=None AND all 14 f_* == 0  (codebook fully silent)
  T2 NO-FRAME Story=None AND CTA=None AND all 11 PERSUASION frames == 0
              (a descriptive image may still fire, but nothing persuasive/narrative)
  T3 NO-ARC   Story=None AND CTA=None (no narrative, no ask; frames may fire)

Token-free (Python = data-generation). Writes the T2 set (the oversample target)
to output/blindspots.csv with Post_ID + genre metadata.

Run with the NLP venv:
  "../4. NLP Pipeline/.venv/bin/python" a5_codebook_blindspots.py
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
import pandas as pd

BASE_DIR = os.path.dirname(os.path.abspath(__file__))
OUT_DIR = os.path.join(BASE_DIR, "output")
os.makedirs(OUT_DIR, exist_ok=True)

# The 11 PERSUASION frames (something is being argued / a device is used) vs. the 3
# purely descriptive VISUAL frames. Splitting them matters here: a post can have a
# visual frame (e.g. on-screen text) yet still be a codebook "blind spot" for persuasion.
PERSUASION = [
    "f_Empathy", "f_Threat", "f_Efficacy", "f_Collective_ID", "f_Normative",
    "f_Moral", "f_Economic", "f_Scientific", "f_Youth_Addressed",
    "f_Interactivity", "f_Youth_Style",
]
VISUAL = ["f_Protagonist", "f_Onscreen_Text", "f_Data_Visual"]
ALL_FRAMES = PERSUASION + VISUAL


def profile(df, label):
    """Print a compact descriptive profile of a blind-spot tier (df): its size, and how
    it breaks down by org, format, topic and BERTopic, with caption length / engagement
    shown against the corpus median so the tier can be compared to the whole. Reads the
    module-level N (coded corpus size) and base (the full coded table) as reference."""
    print(f"\n### {label}  |  n={len(df)}  ({len(df)/N*100:.1f}% of coded corpus)")
    if len(df) == 0:
        return   # nothing to profile for an empty tier
    # Who/what these blind-spot posts are: top organisations, formats, coded topics, clusters.
    print("  by org      : " + ", ".join(
        f"{k} {v}" for k, v in df["Actor_Name"].value_counts().head(6).items()))
    print("  by format   : " + ", ".join(
        f"{k} {v}" for k, v in df["Post_Format"].value_counts().items()))
    print("  by m_Topic  : " + ", ".join(
        f"{k} {v}" for k, v in df["m_Primary_Topic"].fillna("None").value_counts().head(6).items()))
    print("  by BERTopic : " + ", ".join(
        f"T{k} {v}" for k, v in df["BERTopic"].value_counts().head(8).items()))
    # Compare this tier's caption length and engagement to the corpus median (is the
    # blind-spot content shorter/longer, more/less engaging than average?).
    print(f"  cap_chars   : median {df['cap_chars'].median():.0f}  "
          f"(vs corpus {base['cap_chars'].median():.0f})")
    print(f"  eng_rate    : median {df['eng_rate'].median()*100:.2f}%  "
          f"(vs corpus {base['eng_rate'].median()*100:.2f}%)")
    # A few flags: do these posts at least link out, ask a question, or carry on-screen text?
    print(f"  has_link    : {df['has_link'].mean()*100:.0f}%  |  "
          f"has_question {df['has_question'].mean()*100:.0f}%  |  "
          f"Onscreen_Text {df['f_Onscreen_Text'].mean()*100:.0f}%")


# Work on the coded posts only (the blind-spot idea only makes sense where the
# codebook was actually applied).
base = pd.read_csv(os.path.join(BASE_DIR, "analysis_base.csv"), sep=";")
base = base[base["coded"] == 1].copy()  # 917 coded posts
N = len(base)

# Four boolean masks (one True/False per post). Blank categorical cells read as NaN,
# so isna() flags "no story arc" / "no call to action". For the frame masks, blank
# frame cells become NaN which pandas .sum skips, so a row of all-blank frames sums
# to 0 and counts as "no frame" (fine for coded posts, whose codes are present).
no_story = base["m_Story_Structure"].isna()          # no narrative structure coded
no_cta = base["m_Primary_CTA"].isna()                # no ask / call to action coded
no_persuasion = (base[PERSUASION].sum(axis=1) == 0)  # none of the 11 persuasion frames fired
no_any_frame = (base[ALL_FRAMES].sum(axis=1) == 0)   # none of the 14 frames fired at all

# The three nested tiers (T1 strictest -> T3 loosest), combining the masks with & (AND).
t1 = base[no_story & no_cta & no_any_frame]    # codebook completely silent
t2 = base[no_story & no_cta & no_persuasion]   # nothing persuasive/narrative (a visual frame may still fire)
t3 = base[no_story & no_cta]                    # no arc and no ask (persuasion frames may fire)

print(f"# Codebook blind-spots  (N={N} coded posts)")
print(f"\nStory=None: {no_story.sum()} ({no_story.mean()*100:.0f}%)  |  "
      f"CTA=None: {no_cta.sum()} ({no_cta.mean()*100:.0f}%)  |  "
      f"no persuasion frame: {no_persuasion.sum()} ({no_persuasion.mean()*100:.0f}%)  |  "
      f"no frame at all: {no_any_frame.sum()} ({no_any_frame.mean()*100:.0f}%)")

# Profile each tier. T2 is the target set to oversample when refining the codebook.
profile(t1, "T1 STRICT  (Story=None & CTA=None & ALL 14 frames == 0)")
profile(t2, "T2 NO-FRAME  (Story=None & CTA=None & 0 persuasion frames)  <- OVERSAMPLE TARGET")
profile(t3, "T3 NO-ARC  (Story=None & CTA=None; descriptive frames allowed)")

# What ELSE is true about T2 posts? Only descriptive frames can fire.
# So report the residual visual-frame rates + the emotional-valence mix: the little the
# codebook still captures about content it otherwise cannot describe.
print("\n  T2 residual descriptive frames (what little the codebook does capture):")
for f in VISUAL:
    print(f"    {f:18s} {t2[f].mean()*100:4.0f}%")
print("  T2 m_Emotional_Valence:", dict(t2["m_Emotional_Valence"].fillna("None").value_counts()))

# Export the T2 set (id + genre metadata) so these posts can be pulled for re-coding.
cols = ["Post_ID", "Actor_Name", "Post_Format", "BERTopic",
        "m_Primary_Topic", "cap_chars", "eng_rate", "f_Protagonist",
        "f_Onscreen_Text", "n_comments_scraped"]
t2[cols].to_csv(os.path.join(OUT_DIR, "blindspots.csv"), index=False)
print(f"\nWrote {len(t2)} T2 blind-spot posts -> output/blindspots.csv")
