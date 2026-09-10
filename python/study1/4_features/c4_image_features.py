#!/usr/bin/env python3
"""
Phase C - Theme 2 (data-generation). Cover-image visual features on the FULL coded
corpus (N=917 cached jpgs): brightness, contrast, saturation, colourfulness, warm
palette share, dominant colour, an on-image-text density PROXY, and near-duplicate
detection from the cached CLIP embeddings.

No tesseract is installed, so true OCR is unavailable; text density is approximated
by an edge-energy proxy (text_edge_density) and should be read alongside the
validated LLM f_Onscreen_Text flag, not as literal character counts.

Reads : ../3. Codebook & Coding/media_test/<Post_ID>.jpg, analysis_base.csv,
        output/clip_image_emb.npy, output/clip_image_ids.txt
Writes: output/image_features.csv (per-post), output/image_near_duplicates.csv
Run   : "../4. NLP Pipeline/.venv/bin/python" c4_image_features.py
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
# WHY image features: the cover image is the scroll-stopping first impression. These
# low-level aesthetics (how bright/saturated/warm/colourful, how busy with edges,
# whether it reuses a template) describe the VISUAL craft of conservation posts, to
# be compared against framing/engagement in R. All descriptive and exploratory.
import os
import numpy as np
import pandas as pd
from PIL import Image          # Pillow: open + resize + colour-space conversion of the jpgs
from collections import Counter

HERE = os.path.dirname(os.path.abspath(__file__))

# Package layout. The constants above describe the original working tree, where
# the corpus and the feature tables sat in sibling folders. _paths.py knows where
# the same files live here, so the three lines below repoint them.
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
IMG = os.path.join(UP, "3. Codebook & Coding/media_test")   # folder of cached cover jpgs, one per Post_ID
DUP_T = 0.95   # cosine threshold for near-duplicate images

base = pd.read_csv(str(_BASE), sep=";", encoding="utf-8-sig")
coded = base[base["coded"] == 1][["Post_ID", "Actor_Name", "Handle"]].copy()   # only coded posts; keep id + org labels

# Compute a bundle of colour / craft statistics for one post's cover image.
# Returns None if the jpg is missing or unreadable (so the caller can tally it).
def colour_stats(pid):
    path = os.path.join(IMG, pid + ".jpg")
    if not os.path.exists(path):
        return None                                    # no cached image for this post
    try:
        im = Image.open(path).convert("RGB")           # force 3-channel RGB (handles greyscale/CMYK/PNG-alpha)
    except Exception:
        return None                                    # corrupt/unreadable file
    small = im.resize((160, 160))                      # downsample to a fixed size: fast + size-independent stats
    a = np.asarray(small, dtype=np.float32)            # H x W x 3 pixel array
    R, G, B = a[..., 0], a[..., 1], a[..., 2]          # split the colour channels
    lum = 0.299 * R + 0.587 * G + 0.114 * B            # perceptual luminance (Rec. 601 weights)
    # HSV for saturation + hue (warm share)
    hsv = np.asarray(small.convert("HSV"), dtype=np.float32)
    H, S = hsv[..., 0], hsv[..., 1]                    # hue (0-255) and saturation (0-255)
    hue_deg = H * 360.0 / 255.0                        # rescale hue to the familiar 0-360 degree wheel
    warm = ((hue_deg <= 60) | (hue_deg >= 300))        # reds/oranges/yellows/magentas = "warm" pixels
    # Hasler-Susstrunk colourfulness
    # A standard perceptual "colourfulness" metric from opponent-colour channels:
    # rg = red-green, yb = yellow-blue. High values = vivid, varied colour.
    rg = R - G
    yb = 0.5 * (R + G) - B
    colourfulness = float(np.sqrt(rg.std()**2 + yb.std()**2) + 0.3 * np.sqrt(rg.mean()**2 + yb.mean()**2))
    # dominant colour via adaptive palette quantisation
    # Reduce to a 6-colour adaptive palette, then find the most common palette index =
    # the image's dominant colour; report it as hex plus the share of pixels it covers.
    q = small.quantize(colors=6, method=Image.FASTOCTREE)
    pal = q.getpalette()                               # flat [r,g,b, r,g,b, ...] palette table
    counts = Counter(q.getdata())                      # how many pixels map to each palette index
    idx, cnt = counts.most_common(1)[0]                # the most frequent palette index + its pixel count
    dom = pal[idx*3:idx*3+3]                            # look up that index's RGB triplet
    dom_hex = "#{:02x}{:02x}{:02x}".format(*dom)
    dom_share = cnt / (160 * 160)                      # fraction of the (resized) image that colour occupies
    # text/edge density proxy on grayscale
    # No OCR available: approximate "how much text/graphic overlay" by edge energy.
    # Gradient magnitude high in many pixels -> lots of sharp edges (text, charts).
    # This is a PROXY, validated against the LLM onscreen-text flag in the audit below.
    gray = np.asarray(small.convert("L"), dtype=np.float32)
    gx, gy = np.gradient(gray)                          # horizontal + vertical intensity gradients
    grad = np.sqrt(gx**2 + gy**2)                       # gradient magnitude per pixel
    edge_density = float((grad > 40).mean())            # share of pixels above a "strong edge" threshold
    return {
        "brightness": round(float(lum.mean()) / 255, 4),   # mean luminance, 0-1
        "contrast": round(float(lum.std()) / 255, 4),      # luminance spread (std), 0-1 = tonal contrast
        "saturation": round(float(S.mean()) / 255, 4),     # mean colour intensity, 0-1
        "colourfulness": round(colourfulness, 2),
        "warm_share": round(float(warm.mean()), 4),        # fraction of warm-hued pixels
        "dominant_hex": dom_hex,
        "dominant_share": round(dom_share, 4),
        "text_edge_density": round(edge_density, 4),
        "aspect_ratio": round(im.size[0] / im.size[1], 3),   # width/height of the ORIGINAL (not resized) image
    }

# ---- per-image colour + text features -----------------------------------------
# Loop over every coded post, compute its image stats, and tally the ones with no
# cached jpg (missing) so the audit can report coverage.
rows, missing = [], 0
for pid in coded["Post_ID"]:
    s = colour_stats(pid)
    if s is None:
        missing += 1
        continue
    s["Post_ID"] = pid
    rows.append(s)
feat = pd.DataFrame(rows)

# ---- near-duplicate detection from cached CLIP embeddings ----------------------
# Reuse pre-computed CLIP image embeddings (a vector per image capturing its visual
# content). Two images are "near-duplicate" if their embeddings point in almost the
# same direction (cosine similarity >= DUP_T) - i.e. recycled templates / reposts.
emb = np.load(os.path.join(OUT, "clip_image_emb.npy"))   # matrix: one row-vector per image
ids = [l.strip() for l in open(os.path.join(OUT, "clip_image_ids.txt"), encoding="utf-8") if l.strip()]   # row-aligned Post_IDs
assert emb.shape[0] == len(ids), f"emb {emb.shape[0]} != ids {len(ids)}"   # embeddings and ids must line up row-for-row
E = emb / (np.linalg.norm(emb, axis=1, keepdims=True) + 1e-9)   # L2-normalise each vector (+1e-9 avoids /0) so dot product = cosine
S = E @ E.T                                              # full NxN cosine-similarity matrix
np.fill_diagonal(S, -1.0)                                # blank out self-similarity (a row's own diagonal = 1) so it is never its own NN
nn_idx = S.argmax(1)                                     # for each image, the index of its most-similar OTHER image
nn_cos = S.max(1)                                        # that best cosine value
id2org = dict(zip(coded["Post_ID"], coded["Handle"].astype(str).str.lower()))   # Post_ID -> org handle (for same-org checks)

# Per-image nearest-neighbour table, with a 0/1 flag for "is this a near-duplicate?".
nn = pd.DataFrame({
    "Post_ID": ids,
    "nn_post_id": [ids[j] for j in nn_idx],
    "nn_cosine": np.round(nn_cos, 4),
}).assign(is_near_dup=lambda x: (x.nn_cosine >= DUP_T).astype(int))

# duplicate pairs (upper triangle) + connected-component cluster ids
# Enumerate every UNIQUE image pair above threshold. triu_indices(k=1) gives the upper
# triangle only, so each pair (a,b) is counted once (not also as (b,a)).
pairs = []
iu = np.triu_indices(len(ids), k=1)
mask = S[iu] >= DUP_T
for a, b in zip(iu[0][mask], iu[1][mask]):
    pairs.append((ids[a], ids[b], round(float(S[a, b]), 4),
                  int(id2org.get(ids[a]) == id2org.get(ids[b]))))   # same_org flag: recycled WITHIN one org vs. across orgs
pairs_df = pd.DataFrame(pairs, columns=["id_a", "id_b", "cosine", "same_org"])
pairs_df.to_csv(os.path.join(OUT, "image_near_duplicates.csv"), index=False)

# connected components -> cluster id
# Group images that are transitively near-duplicates (a~b, b~c => {a,b,c}) into
# clusters using a union-find. Each near-duplicate pair unions its two members.
import collections
parent = {i: i for i in ids}          # union-find: every image starts as its own parent
def find(x):                          # find the cluster root of x, with path compression
    while parent[x] != x:
        parent[x] = parent[parent[x]]; x = parent[x]
    return x
for a, b, *_ in pairs:                # union the two ends of every duplicate pair
    parent[find(a)] = find(b)
comp = collections.defaultdict(list)  # gather members by their root
for i in ids:
    comp[find(i)].append(i)
# Number only the clusters with >1 member (true duplicate groups); map each member to
# its cluster id. Singletons get no id (NaN).
dup_clusters = {i: k for k, (root, members) in enumerate(
    (r, m) for r, m in comp.items() if len(m) > 1) for i in members}
nn["dup_cluster"] = nn["Post_ID"].map(dup_clusters).astype("Int64")   # nullable int: blank for non-duplicates

# Join colour features + near-dup table + org labels into one per-post image table.
feat = feat.merge(nn, on="Post_ID", how="left").merge(coded, on="Post_ID", how="left")
feat.to_csv(os.path.join(OUT, "image_features.csv"), index=False)

# ---- audit --------------------------------------------------------------------
# Console summary: coverage, duplicate counts, median aesthetics, the proxy-vs-LLM
# text-density check, and the biggest recycled-template clusters.
print(f"c4_image_features.py: {len(feat)} images processed ({missing} missing) -> image_features.csv")
print(f"  near-duplicate pairs (cosine>={DUP_T}): {len(pairs_df)} "
      f"| images flagged near-dup: {int(nn.is_near_dup.sum())} "
      f"| dup clusters: {nn.dup_cluster.nunique()}")
print(f"  within-org dup pairs: {int(pairs_df.same_org.sum())}/{len(pairs_df)} "
      f"(recycled templates within an org)")
print("\nColour / craft (median):")
print("  brightness {:.2f} | contrast {:.2f} | saturation {:.2f} | colourfulness {:.0f} | warm_share {:.2f}".format(
    feat.brightness.median(), feat.contrast.median(), feat.saturation.median(),
    feat.colourfulness.median(), feat.warm_share.median()))
print("  text_edge_density (proxy) median {:.3f}".format(feat.text_edge_density.median()))
# proxy validation vs LLM onscreen-text flag
# If the edge proxy tracks real on-image text, its median should be HIGHER for posts
# the LLM flagged as having onscreen text (f_Onscreen_Text = 1) than for those it did not.
val = feat.merge(base[["Post_ID", "f_Onscreen_Text"]], on="Post_ID")
val["f_Onscreen_Text"] = pd.to_numeric(val.f_Onscreen_Text, errors="coerce")
grp = val.groupby("f_Onscreen_Text").text_edge_density.median()
print("\n  text_edge_density by LLM f_Onscreen_Text (proxy check):")
print(grp.round(3).to_string())
print("\nTop near-duplicate clusters (recycled cover images):")
# The 5 largest duplicate clusters + which org they belong to (template reuse).
top = nn.dropna(subset=["dup_cluster"]).groupby("dup_cluster").size().sort_values(ascending=False).head(5)
for cl, sz in top.items():
    members = nn[nn.dup_cluster == cl].Post_ID.tolist()
    org = id2org.get(members[0], "?")
    print(f"  cluster {int(cl)}: {sz} images, org={org}")