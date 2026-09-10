#!/usr/bin/env python3
"""
Phase A.3 - CLIP visual clustering / visual-genre labelling (local, no LLM tokens).

Encodes the cached post images with clip-ViT-B-32 (sentence-transformers) and
derives a VISUAL genre for every post two ways:

  (a) ZERO-SHOT  - cosine-match each image against a fixed set of visual-genre
      text prompts (animal portrait / landscape / people-and-event / poster-text /
      infographic / behind-the-scenes / branding-logo / product-or-object).
      Interpretable, named, directly usable as a stratification axis.
  (b) HDBSCAN     - unsupervised clusters over a UMAP projection of the image
      embeddings. Data-driven; catches structure the fixed labels miss.

Caches the image embeddings to output/clip_image_emb.npy (+ ids) so re-runs are
instant. Writes output/visual_clusters.csv (Post_ID, visual_zeroshot,
visual_zeroshot_margin, visual_hdbscan). Cross-tabs visual genre x BERTopic genre
x format to stdout.

Run with the NLP venv:
  "../4. NLP Pipeline/.venv/bin/python" a3_clip_visual_clusters.py
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
import os
import numpy as np    # vector maths on the image embeddings
import pandas as pd   # tables + crosstabs
from PIL import Image, ImageFile
ImageFile.LOAD_TRUNCATED_IMAGES = True   # tolerate a few truncated cached jpgs

# Paths from __file__ so the script runs from anywhere.
BASE_DIR = os.path.dirname(os.path.abspath(__file__))
CA_DIR = os.path.abspath(os.path.join(BASE_DIR, ".."))   # "1. Account & Content Analysis"
MEDIA = os.path.join(CA_DIR, "3. Codebook & Coding", "media_test")   # cached post images (<Post_ID>.jpg)
OUT_DIR = os.path.join(BASE_DIR, "output")
os.makedirs(OUT_DIR, exist_ok=True)   # create output/ on first run
# Cache the (slow) image embeddings + their ids so re-runs skip re-encoding.
EMB_CACHE = os.path.join(OUT_DIR, "clip_image_emb.npy")
ID_CACHE = os.path.join(OUT_DIR, "clip_image_ids.txt")

# Visual-genre prompts. Each genre gets a few paraphrases; we average their
# embeddings into one prototype vector (more robust than a single phrase).
VISUAL_PROMPTS = {
    "animal_portrait": [
        "a close-up photograph of a wild animal",
        "a photograph of an elephant, rhino, giraffe, cheetah or seal in nature",
        "a wildlife photograph of a single animal",
    ],
    "landscape_scenery": [
        "a scenic landscape photograph of nature",
        "a photograph of a desert, dunes, ocean or savannah with no people",
        "an empty wilderness or lodge scenery photograph",
    ],
    "people_event": [
        "a photograph of a group of people at an event or meeting",
        "people posing together for a group photo",
        "a workshop, ceremony or field-team photograph with several people",
    ],
    "person_portrait": [
        "a portrait photograph of one person",
        "a headshot of a single staff member, ranger or volunteer",
    ],
    "poster_text": [
        "a graphic poster with large text overlaid on the image",
        "a flyer or announcement card with a headline and text",
        "a social media graphic that is mostly text on a coloured background",
    ],
    "infographic_data": [
        "an infographic with charts, numbers, statistics or a map",
        "a data visualisation or diagram",
    ],
    "behind_the_scenes": [
        "a candid behind-the-scenes photograph of conservation field work",
        "people doing hands-on work such as collaring, rescuing or planting",
    ],
    "branding_logo": [
        "a logo or brand identity graphic",
        "a decorative branded image with a logo and slogan",
    ],
}


def load_or_encode(model, ids_wanted):
    """Return the CLIP image-embedding matrix for ids_wanted, using the on-disk cache
    when it exactly matches, otherwise encoding fresh and rewriting the cache.

    Each row is one image turned into a normalised vector (unit length, so a dot
    product between two rows equals their cosine similarity). Rows are in ids order.
    """
    # Fast path: reuse the cache ONLY if it covers exactly the same ids in the same
    # order (a partial/stale cache would misalign rows to posts, so re-encode instead).
    if os.path.exists(EMB_CACHE) and os.path.exists(ID_CACHE):
        cached_ids = open(ID_CACHE).read().split()
        if cached_ids == ids_wanted:
            print("loaded cached image embeddings", flush=True)
            return np.load(EMB_CACHE)
    # Slow path: open each image and encode it. Unreadable files are skipped, so the
    # returned matrix may cover fewer posts than ids_wanted (see _ENCODED_IDS below).
    print("encoding images (one-time)...", flush=True)
    ids, imgs = [], []
    for pid in ids_wanted:
        p = os.path.join(MEDIA, pid + ".jpg")
        try:
            im = Image.open(p).convert("RGB"); im.load()
            ids.append(pid); imgs.append(im)
        except Exception as e:
            print(f"  skip {pid}: {e}", flush=True)
    # normalize_embeddings=True -> unit vectors, so later "emb @ protos.T" is cosine similarity.
    emb = model.encode(imgs, batch_size=16, convert_to_tensor=False,
                       show_progress_bar=True, normalize_embeddings=True)
    emb = np.asarray(emb, dtype=np.float32)
    np.save(EMB_CACHE, emb)                       # cache the matrix ...
    open(ID_CACHE, "w").write("\n".join(ids))     # ... and the exact ids/order it corresponds to
    globals()["_ENCODED_IDS"] = ids               # publish the actually-encoded ids back to main() (drops the skipped ones)
    return emb


def main():
    from sentence_transformers import SentenceTransformer   # imported inside main so the module loads even without torch

    # Start from the joined base table, keep only coded posts, then keep only those
    # whose image is actually on disk (we cannot embed a picture we do not have).
    base = pd.read_csv(os.path.join(BASE_DIR, "analysis_base.csv"), sep=";")
    base = base[base["coded"] == 1].copy()
    # only posts whose image actually exists on disk
    have = [pid for pid in base["Post_ID"]
            if os.path.exists(os.path.join(MEDIA, pid + ".jpg"))]
    print(f"{len(have)} posts have a cached image", flush=True)

    print("loading clip-ViT-B-32...", flush=True)
    model = SentenceTransformer("clip-ViT-B-32")

    # _ENCODED_IDS defaults to `have`; load_or_encode overwrites it with the truly
    # encoded ids if any image failed. emb[:len(ids)] then trims the matrix to match.
    globals()["_ENCODED_IDS"] = have
    emb = load_or_encode(model, have)
    ids = globals()["_ENCODED_IDS"]
    emb = emb[: len(ids)]
    print(f"embeddings: {emb.shape}", flush=True)   # (N images, 512 dims for ViT-B-32)

    # ---- (a) zero-shot visual genre ----
    # "Zero-shot" = classify images into named genres WITHOUT training a classifier:
    # embed the genre's text prompts, average them into one prototype vector per genre,
    # then assign each image to whichever prototype it is most cosine-similar to.
    genre_names = list(VISUAL_PROMPTS)
    protos = []
    for g in genre_names:
        t = model.encode(VISUAL_PROMPTS[g], convert_to_numpy=True,
                         normalize_embeddings=True)   # encode this genre's few paraphrases
        v = t.mean(axis=0)                            # average them -> one prototype (robust to phrasing)
        v = v / np.linalg.norm(v)                     # renormalise to unit length so dot product = cosine
        protos.append(v)
    protos = np.vstack(protos)                      # (G, d)  one row per genre prototype
    sims = emb @ protos.T                            # (N, G) cosine (both normed): image x genre similarity
    top = sims.argmax(axis=1)                        # index of the best-matching genre per image
    srt = np.sort(sims, axis=1)                      # sort each row's similarities ascending
    margin = srt[:, -1] - srt[:, -2]                 # confidence gap top1-top2 (small = ambiguous assignment)
    zeroshot = [genre_names[i] for i in top]         # map the winning index back to the genre name

    # ---- (b) HDBSCAN over UMAP ----
    # Complementary, data-driven view: instead of imposing genre labels, let the images
    # cluster themselves. UMAP first squashes the 512-d embeddings down to 10 dims
    # (denoise + make density meaningful), then HDBSCAN finds dense groups and marks
    # the rest as noise (label -1). random_state=42 makes UMAP reproducible.
    import umap, hdbscan
    reducer = umap.UMAP(n_neighbors=15, n_components=10, min_dist=0.0,
                        metric="cosine", random_state=42)   # cosine metric: matches how CLIP vectors compare
    proj = reducer.fit_transform(emb)
    # min_cluster_size=12: a group needs >=12 images to count as a cluster (else noise);
    # min_samples=5 controls how conservative/tight the clustering is.
    clusterer = hdbscan.HDBSCAN(min_cluster_size=12, min_samples=5,
                                metric="euclidean")
    hdb = clusterer.fit_predict(proj)                # per-image cluster id, -1 = noise/unclustered

    # One row per image with both labellings; ids order matches emb/zeroshot/hdb order.
    res = pd.DataFrame({
        "Post_ID": ids,
        "visual_zeroshot": zeroshot,
        "visual_zeroshot_margin": np.round(margin, 4),
        "visual_hdbscan": hdb,
    })
    res.to_csv(os.path.join(OUT_DIR, "visual_clusters.csv"), index=False)

    m = base.merge(res, on="Post_ID", how="inner")   # attach codebook vars for the profiling below

    # For each zero-shot genre: how many posts, and its typical make-up (protagonist %,
    # on-screen-text %, share that are Video, median caption length, median engagement).
    print("\n# Zero-shot visual genre distribution")
    for g, n in res["visual_zeroshot"].value_counts().items():
        sub = m[m["visual_zeroshot"] == g]
        print(f"  {g:18s} {n:4d} ({n/len(res)*100:4.1f}%)  "
              f"Protag {sub['f_Protagonist'].mean()*100:3.0f}%  "
              f"OnscrTxt {sub['f_Onscreen_Text'].mean()*100:3.0f}%  "
              f"Video {(sub['Post_Format']=='Video').mean()*100:3.0f}%  "
              f"cap {sub['cap_chars'].median():.0f}  "
              f"eng {sub['eng_rate'].median()*100:.2f}%")
    # Overall assignment confidence: median top1-top2 gap, and the fraction of images
    # whose winning genre barely beat the runner-up (<0.01 = essentially a toss-up).
    print(f"  (median top1-top2 margin {np.median(margin):.3f}; "
          f"{(margin<0.01).mean()*100:.0f}% low-confidence < 0.01)")

    # Describe the data-driven clusters. Cluster count excludes the -1 noise label.
    print("\n# HDBSCAN unsupervised clusters (visual)")
    print(f"  {len(set(hdb))-(1 if -1 in hdb else 0)} clusters, "
          f"{(hdb==-1).sum()} noise ({(hdb==-1).mean()*100:.0f}%)")
    for c in sorted(set(hdb)):
        sub = m[m["visual_hdbscan"] == c]
        if len(sub) < 8 and c != -1:      # skip tiny clusters in the printout (keep the noise bin)
            continue
        # Characterise each cluster by what it is made of: its top zero-shot genres,
        # top BERTopic (caption) topics, and the orgs that dominate it. Agreement
        # between the two labellings is a sign the visual genre is real, not an artefact.
        zs = sub["visual_zeroshot"].value_counts()
        bt = sub["BERTopic"].value_counts().head(3)
        org = sub["Actor_Name"].value_counts().head(2)
        tag = "NOISE" if c == -1 else f"V{c}"
        print(f"  {tag:6s} n={len(sub):3d}  zeroshot={dict(zs.head(3))}  "
              f"BERTopic={{{', '.join(f'T{k}:{v}' for k,v in bt.items())}}}  "
              f"org={dict(org)}")

    # Crosstab the caption-topic axis (BERTopic) against the visual-genre axis, to read
    # off which visuals accompany a few emblematic topics (do text genre and image genre align?).
    print("\n# Zero-shot visual genre x BERTopic genre (top pairings)")
    ct = pd.crosstab(m["BERTopic"], m["visual_zeroshot"])
    # show, for a few emblematic BERTopics, their visual make-up
    for t in [1, 2, 3, 8, 6, 14, 20, 12, 24]:
        if t in ct.index:
            row = ct.loc[t].sort_values(ascending=False)   # visual genres for this topic, most common first
            row = row[row > 0]                              # drop empty cells
            print(f"  T{t:<3d}: " + ", ".join(f"{k} {v}" for k, v in row.head(4).items()))

    print(f"\nWrote {len(res)} rows -> output/visual_clusters.csv")


if __name__ == "__main__":
    main()