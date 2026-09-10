#!/usr/bin/env python3
"""
Build ONE tidy per-post table joining every saved source, so the R scripts read
a single file (Python = data-generation, R = inference). Output: analysis_base.csv

Joins: NLP corpus (cluster id/likes/comments/caption) + multimodal LLM
(19 vars incl. visual) + Apify profiles (followers -> engagement rate) + CLIP
alignment + Apify comments (count + mean VADER sentiment per post) + derived
caption metrics. Run with the NLP venv python.
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
# Status: RECORD ONLY. It cannot run here, because it needs the NLP cluster output
#         and the raw Apify JSON, and reads them from the collection folders of the
#         working environment it was written in.
#
# Standard library only here: json/csv to read the source files, re for the small
# regex-based caption features, statistics (as st) for medians/means, os for paths.
import json, csv, re, statistics as st, os
from collections import defaultdict          # a dict that auto-creates an empty list for any new key
# VADER = a rule/lexicon sentiment scorer built for short social-media text.
# It returns a "compound" score in [-1, +1] per string; used below on comments.
from nltk.sentiment.vader import SentimentIntensityAnalyzer

# HERE = this script's own folder; UP = its parent ("1. Account & Content Analysis"),
# under which every source CSV/JSON lives. Building paths from __file__ means the
# script runs correctly no matter what the current working directory is.
HERE = os.path.dirname(os.path.abspath(__file__)); UP = os.path.join(HERE, "..")
# rr = "read rows": open a SEMICOLON-delimited CSV and return it as a list of dicts
# (one dict per row, keyed by the header). The list comprehension drops any line
# starting with "#" (the exports carry a "#"-prefixed metadata header). utf-8-sig
# quietly swallows a leading byte-order-mark if one is present.
def rr(p):
    return list(csv.DictReader([l for l in open(p, encoding="utf-8-sig") if not l.startswith("#")], delimiter=";"))
# I = safe integer cast: parse "1234" or "1234.0" to int; return 0 on anything
# unparseable (blank cell, "NA", None) so the downstream arithmetic never crashes.
def I(x):
    try: return int(float(x))
    except: return 0
# emojis = count emoji characters by matching several Unicode emoji blocks.
# `s or ''` guards against a None caption (would crash re.findall otherwise).
def emojis(s): return len(re.findall(r'[\U0001F000-\U0001FAFF☀-➿←-⇿⬀-⯿]', s or ''))
# shortcode = pull the post's Instagram shortcode out of a ".../p/<code>/" URL,
# used to join scraped comments back to their post; None if the URL has no /p/ part.
def shortcode(u):
    m = re.search(r"/p/([^/]+)/", u or ""); return m.group(1) if m else None

# --- Load every source, keyed for O(1) lookup where a join is needed ---
# NLP = the caption-level NLP framing table, one row per post (frame scores,
# predicted categoricals, topics, Likes/Comments, Caption). This is the ROW SPINE:
# the build loop below iterates over NLP and everything else is joined onto it.
NLP = rr(UP + "/4. NLP Pipeline/NLP_Analysis/Study1_NLP_Framing_2026-06-10_0828.csv")
# MM = multimodal LLM codes, turned into a dict keyed by Post_ID so a post's codes
# are one lookup away. Holds the MANUAL_<var> columns (the 19 coded vars incl. visual).
MM  = {r["Post_ID"]: r for r in rr(UP + "/3. Codebook & Coding/Study1_LLM_multimodal_920.csv")}
# CLIP = per-post image-caption alignment score produced by a2 (Post_ID -> clip_align).
CLIP = {r["Post_ID"]: r["clip_align"] for r in rr(HERE + "/clip_alignment.csv")}
# PROF = Apify profile scrape (a JSON list of account objects). foll collapses it to
# username(lowercased) -> follower count, used as the engagement-rate denominator.
PROF = json.load(open(UP + "/2. Data Collection (Apify)/Profile Data Apify/dataset_instagram-profile-scraper_2026-07-03_08-53-11-324.json", encoding="utf-8"))
foll = {x["username"].lower(): x["followersCount"] for x in PROF}
# COM = all scraped comments (JSON list); used next to derive per-post sentiment.
COM = json.load(open(UP + "/2. Data Collection (Apify)/Comment Data Apify/comments_merged.json", encoding="utf-8"))

# Build the VADER analyzer once (loading its lexicon is the slow part; keep it out
# of the loop). csent maps Post_ID -> list of the compound sentiment scores of that
# post's comments.
sia = SentimentIntensityAnalyzer()
csent = defaultdict(list)
for c in COM:
    pid = shortcode(c.get("postUrl"))   # which post does this comment belong to?
    # If we recovered a post id, score this comment and append its compound value
    # (-1 = very negative .. +1 = very positive) to that post's running list.
    if pid: csent[pid].append(sia.polarity_scores(c.get("text") or "")["compound"])

# The codebook variable groups (each name maps to a MANUAL_<name> column in MM):
BIN = ["Empathy","Threat","Efficacy","Collective_ID","Normative","Moral","Economic","Scientific","Youth_Addressed","Interactivity"]  # 10 binary persuasion/frame vars (0/1)
VIS = ["Youth_Style","Protagonist","Onscreen_Text","Data_Visual"]                                                                    # 4 binary VISUAL (image-only) vars
MUL = ["Emotional_Valence","Story_Structure","Primary_CTA","Primary_Topic","Language"]                                               # 5 multiclass (categorical) vars

# Walk every post in the NLP spine and assemble one flat output dict per post.
out_rows = []
for r in NLP:
    pid = r["Post_ID"]; mm = MM.get(pid, {}); cap = r.get("Caption_Text") or ""   # id; this post's LLM codes ({} if unmatched); caption ("" if missing)
    h = (r.get("Instagram_Handle") or "").lstrip("@").lower()   # normalise handle: drop leading "@", lowercase -> matches the follower-dict keys
    f = foll.get(h); likes = I(r.get("Likes"))                  # f = follower count (None if org not scraped); likes as a safe int
    row = {
        # --- identity / metadata carried straight through from the NLP row ---
        "Post_ID": pid, "Actor_Name": r.get("Actor_Name"), "Actor_Type": r.get("Actor_Type"),
        "Handle": h, "Post_Format": r.get("Post_Format"), "Posting_Date": r.get("Posting_Date"),
        # --- engagement counts + size-normalised rate ---
        "Likes": likes, "Comments": I(r.get("Comments")), "Followers": f or "",   # Followers blank ("") when unknown
        # eng_rate = likes / followers: a follower-size-normalised engagement proxy so
        # big and small accounts are comparable. Blank when followers unknown (no /0).
        # Note: uses LIKES only, not comments, by design.
        "eng_rate": round(likes / f, 6) if f else "",
        # --- derived caption metrics (cheap text features, computed once here) ---
        "cap_chars": len(cap), "cap_words": len(cap.split()),                       # caption length in chars and whitespace-split words
        "n_hashtags": cap.count("#"), "n_mentions": cap.count("@"), "n_emojis": emojis(cap),   # raw "#"/"@" counts + emoji count
        # has_link = 1 if the caption looks like it points somewhere (URL, ".com", or
        # "link in bio"); re.I = case-insensitive. A crude call-to-click flag.
        "has_link": int(bool(re.search(r'https?://|\.com|link in bio', cap, re.I))),
        "has_question": int("?" in cap),                                           # 1 if any "?" appears
        # --- joined-in features from the other sources ---
        "clip_align": CLIP.get(pid, ""),                                           # image-caption similarity from a2 ("" if none)
        "n_comments_scraped": len(csent.get(pid, [])),                             # how many comments we captured for this post
        "comment_sent_mean": round(st.mean(csent[pid]), 4) if csent.get(pid) else "",   # mean VADER compound over those comments ("" if none)
        # coded = 1 if this post carries a manual/LLM code (MANUAL_Empathy is "0" or "1").
        # This is the GATE the later a-scripts filter on (they keep coded == 1 only).
        "coded": int(mm.get("MANUAL_Empathy") in ("0", "1")),
    }
    # Copy the multimodal LLM codes onto the row, namespaced by source: "f_" for the
    # 14 binary frame/visual vars, "m_" for the 5 categoricals. Blank ("") if uncoded.
    for v in BIN + VIS:
        row["f_" + v] = mm.get("MANUAL_" + v, "")
    for v in MUL:
        row["m_" + v] = mm.get("MANUAL_" + v, "")
    # The inductive BERTopic cluster id, the stratum the exploratory subsample was
    # drawn on and the scheme behind the per-organization content mix.
    row["BERTopic"] = r.get("BERTopic_cluster", "")
    out_rows.append(row)

# Write the single joined table as a semicolon CSV (utf-8-sig so Excel/R open it
# cleanly). Column order/header comes from the first row's keys, which is safe here
# because every row was built with the identical set of keys.
OUT = HERE + "/analysis_base.csv"
with open(OUT, "w", encoding="utf-8-sig", newline="") as fh:
    w = csv.DictWriter(fh, fieldnames=list(out_rows[0].keys()), delimiter=";")
    w.writeheader(); w.writerows(out_rows)
# Coverage tallies for the console summary: how many posts are coded, have a follower
# count, have comment sentiment (and CLIP below) - a quick check the joins landed.
coded = sum(r["coded"] for r in out_rows)
withf = sum(1 for r in out_rows if r["Followers"] != "")
withc = sum(1 for r in out_rows if r["comment_sent_mean"] != "")
print(f"Wrote {OUT}")
print(f"  {len(out_rows)} posts | {coded} coded | {withf} with followers | {withc} with comment sentiment | {sum(1 for r in out_rows if r['clip_align']!='')} with CLIP")