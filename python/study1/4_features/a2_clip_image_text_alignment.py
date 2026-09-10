#!/usr/bin/env python3
"""
CLIP image-text ALIGNMENT for Study 1 (local, no LLM tokens).
Loads the open CLIP model 'clip-ViT-B-32' via sentence-transformers, encodes each
post's cached image (media_test/<id>.jpg) and its caption, and stores the cosine
similarity = how well the image matches the caption. Low alignment = the image is
decorative / generic relative to the text (operationalises "image subordinate").

Model source: sentence-transformers/clip-ViT-B-32 on HuggingFace (public, ~600 MB,
downloaded once to the HF cache). Runs on CPU. Output:
  clip_alignment.csv  (Post_ID, has_image, has_caption, clip_align)
Run with the NLP venv python (has torch + sentence_transformers; Pillow required).
"""
#
# PATHS IN THIS PACKAGE
# This script was written inside the thesis working tree, where its inputs sat in
# sibling folders named "2. Data Collection (Apify)" and the like. In the
# reproduction package the same files live under data/, and python/_paths.py
# holds the canonical locations.
#
# Status: RECORD ONLY. It cannot run here, because it needs the scraped post images, which are not shipped (the organizations' own photographs, 917 files, 291 MB).
# The outputs it produced are shipped and are the canonical record, so nothing in
# the reported results depends on rerunning it.
#
import csv, os, sys
from PIL import Image, ImageFile   # Pillow: image loading
ImageFile.LOAD_TRUNCATED_IMAGES = True   # tolerate a few truncated cached jpgs
# sentence-transformers wraps CLIP: SentenceTransformer loads the model, util has cos_sim.
from sentence_transformers import SentenceTransformer, util

# Paths built from __file__ so the script runs regardless of the working directory.
HERE = os.path.dirname(os.path.abspath(__file__))
UP = os.path.join(HERE, "..")
MEDIA = os.path.join(UP, "3. Codebook & Coding", "media_test")   # folder of cached post images (<Post_ID>.jpg)
NLP = os.path.join(UP, "4. NLP Pipeline", "NLP_Analysis", "Study1_NLP_Framing_2026-06-10_0828.csv")   # source of Post_ID + caption
OUT = os.path.join(HERE, "clip_alignment.csv")   # output consumed by 00_build_analysis_base.py

# rr = read a semicolon CSV into a list of dicts, dropping "#"-prefixed header lines.
def rr(p):
    with open(p, encoding="utf-8-sig") as fh:
        return list(csv.DictReader([l for l in fh if not l.startswith("#")], delimiter=";"))

# Keep only posts that have BOTH a usable cached image and a non-empty caption -
# alignment is undefined without both sides. Guards: file exists, >1000 bytes (skip
# tiny/placeholder files), caption non-blank after stripping.
rows = rr(NLP)
posts = []
for r in rows:
    pid = r["Post_ID"]; cap = (r["Caption_Text"] or "").strip()
    img = os.path.join(MEDIA, pid + ".jpg")
    if os.path.exists(img) and os.path.getsize(img) > 1000 and cap:
        posts.append((pid, img, cap[:800]))  # cap at 800 chars; CLIP truncates text anyway
print(f"{len(posts)} posts with image+caption", flush=True)

# CLIP was trained so that an image and a caption describing it land CLOSE together
# in one shared embedding space. That is exactly what lets us score image<->text match.
print("loading clip-ViT-B-32 (first run downloads ~600MB)...", flush=True)
model = SentenceTransformer("clip-ViT-B-32")

# Actually open each image now; drop any that Pillow cannot read (corrupt cache) so
# the ids/imgs/caps lists stay perfectly aligned (same length, same order).
print("loading images (skipping unreadable)...", flush=True)
ids, imgs, caps, bad = [], [], [], 0
for pid, path, cap in posts:
    try:
        im = Image.open(path).convert("RGB"); im.load()   # force pixels into memory now so a later failure cannot desync the lists
        ids.append(pid); imgs.append(im); caps.append(cap)
    except Exception as e:
        bad += 1; print(f"  skip {pid}: {e}", flush=True)
print(f"{len(ids)} usable, {bad} unreadable", flush=True)
# Encode images and captions into the shared CLIP vector space (batched for speed).
# Because ids/imgs/caps are index-aligned, row k of img_emb and row k of txt_emb are
# the SAME post's picture and words.
print("encoding images...", flush=True)
img_emb = model.encode(imgs, batch_size=16, convert_to_tensor=True, show_progress_bar=True)
print("encoding captions...", flush=True)
txt_emb = model.encode(caps, batch_size=32, convert_to_tensor=True, show_progress_bar=True)

# cos_sim builds the full image x caption similarity matrix; .diagonal() keeps only the
# matched (same-post) pairs -> one cosine similarity per post. Cosine near 1 = image
# closely matches its caption; near 0 = image is generic/decorative relative to the text.
sims = util.cos_sim(img_emb, txt_emb).diagonal().tolist()
with open(OUT, "w", encoding="utf-8-sig", newline="") as fh:
    w = csv.writer(fh, delimiter=";"); w.writerow(["Post_ID", "clip_align"])
    for pid, s in zip(ids, sims):
        w.writerow([pid, round(float(s), 4)])   # one row per post: id + rounded alignment

import statistics as st
print(f"\nWrote {OUT}")
# Summary of the alignment distribution + how many posts fall below a low-match cutoff.
print(f"  clip_align: median {st.median(sims):.3f}  mean {st.mean(sims):.3f}  min {min(sims):.3f}  max {max(sims):.3f}")
lo = sum(1 for s in sims if s < 0.20)   # 0.20 is a descriptive threshold for "image weakly tied to the caption"
print(f"  {lo} posts ({100*lo/len(sims):.0f}%) with alignment < 0.20 (image weakly related to caption)")
