#!/usr/bin/env python3
"""
Phase A.4 (draw) - draw the seeded stratified 150-post sample for the bounded
inductive LLM pass (Phase B). Token-free, fully reproducible (seed = 42).

Reads:
  output/sampling_frame.csv   - functional_genre, PCA terciles, new-genre KW flags,
                                comment availability, blindspot membership (917 posts)
  output/visual_clusters.csv  - CLIP zero-shot visual genre per post
  analysis_base.csv           - joined only for the TRUE public Comments count
                                (sampling_frame carries n_comments_scraped, capped 15;
                                 the high-discussion layer needs the uncapped total)

Layered draw (150 total), drawn as DISTINCT overlapping layers then reconciled:
  Layer 1 - genre core        ~100  sqrt-proportional (floor 6) allocation across the
                                    13 functional genres; within each genre picked to
                                    maximise spread over Post_Format x Actor x the 3
                                    PCA terciles
  Layer 2 - blind-spot        +14   extra draws from the T2 no-frame pool, weighted to
                                    ecotourism-aesthetic + weekly-filler the core under-counts
  Layer 3 - high-discussion   +18   posts with >=20 TOTAL public comments, capped <=4/org
                                    so the stance layer is cross-org, not all OCN
  Layer 4 - coverage top-ups  ~+18  only where a floor is unmet:
                                    new genres  commemoration_day>=6, live_series>=5,
                                                hiring>=3, pure_info>=8
                                    visual      infographic_data>=3, branding_logo>=4,
                                                every visual genre>=3
                                    engagement  hi + lo eng_rate post per top-5 genre

Layers are distinct by construction (each draws from the not-yet-selected pool).
The union is then reconciled to EXACTLY 150: pad with extra core if short, trim the
most over-represented core posts if long (priority guarantees > high-discussion >
blind-spot > core; a core post is only removable if its genre / format / PCA-bin /
visual-floor / new-genre-floor / org coverage all survive its removal).

Each selected post is tagged with `selection_reason`. Prints a full coverage audit.
Output: sample_150.csv  (Post_ID + strata + selection_reason + image path + comment counts).
NOTHING here spends Phase B tokens.

Run with the NLP venv:
  "../4. NLP Pipeline/.venv/bin/python" a7_draw_sample_150.py
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
# Status: RECORD ONLY. It cannot run here, because its image_exists column checks
#         the scraped post images on disk, which are not shipped.
#
import os
import numpy as np
import pandas as pd

BASE_DIR = os.path.dirname(os.path.abspath(__file__))
OUT_DIR = os.path.join(BASE_DIR, "output")
MEDIA_REL = os.path.join("..", "3. Codebook & Coding", "media_test")

# ---- tunables (everything re-runnable in seconds) ---------------------------
SEED = 42
N_TOTAL = 150
CORE_TARGET = 100          # nominal core size; sqrt/floor allocation lands near this
CORE_FLOOR = 6             # min core draws per functional genre
OUTLIER_FLOOR = 8          # the atypical long tail gets a slightly higher floor
N_BLINDSPOT = 14           # blind-spot oversample (extra, beyond what core grabs)
BLINDSPOT_BOOST = 2.0      # draw-weight multiplier for the under-counted blind-spot genres
BLINDSPOT_BOOST_GENRES = {"ecotourism_aesthetic", "weekly_ritual_filler"}
N_HIGHDISC = 18            # high-discussion posts for the comment-stance layer
HIGHDISC_MIN_COMMENTS = 20 # TRUE public comment count threshold
HIGHDISC_CAP_PER_ORG = 4   # keep the stance layer cross-org
NEW_GENRE_FLOORS = {"commemoration_day": 6, "live_series": 5, "hiring": 3, "pure_info": 8}
VISUAL_FLOORS = {"infographic_data": 3, "branding_logo": 4}
VISUAL_FLOOR_DEFAULT = 3   # every CLIP visual genre gets at least this many
ENG_EXTREME_TOP_GENRES = 5 # hi + lo eng_rate post for the 5 largest genres
SPREAD_DIMS = ["Post_Format", "Actor_Name", "pca1_bin", "pca2_bin", "pca3_bin"]

# selection-reason priority: higher is kept when a post could belong to several layers,
# and lower is trimmed first when reconciling down to 150.
PRIORITY = {"coverage_guarantee": 4, "high_discussion": 3, "blindspot": 2, "genre_core": 1}

# ONE seeded random generator drives every stochastic choice below (the weighted
# blind-spot draw and the greedy tie-breaks). Seeding it with SEED=42 makes the whole
# 150-post draw fully reproducible - which matters here because this sample becomes the
# HUMAN-coded Cohen's kappa ground truth, so anyone must be able to regenerate the exact
# same 150 posts.
rng = np.random.default_rng(SEED)


def to_bool(s):
    """Coerce a CSV column (booleans come back as text after a round-trip) to real bools:
    True only where the value is the literal 'true' or '1' (case-insensitive), else False."""
    return s.astype(str).str.strip().str.lower().isin(["true", "1"])


def greedy_spread(pool, k, seed_counts=None):
    """Pick k row-labels from `pool` maximising spread over SPREAD_DIMS.

    Greedy min-coverage: repeatedly take the candidate whose strata values are
    currently least represented (summed count over the 5 dims), seeded random
    tie-break. `seed_counts` pre-loads the counters with posts already in the
    sample for this stratum, so the core complements them rather than clumping.
    """
    k = int(min(k, len(pool)))                    # never ask for more posts than the pool holds
    if k <= 0:
        return []
    # counts[dimension][value] = how many already-picked posts carry that stratum value.
    counts = {d: {} for d in SPREAD_DIMS}
    if seed_counts is not None:
        # Pre-load the counters with what the sample already contains for this stratum, so
        # the core COMPLEMENTS the existing picks (fills the gaps) instead of clumping.
        for d in SPREAD_DIMS:
            counts[d].update(seed_counts.get(d, {}))
    remaining = list(pool.index)
    picked = []
    for _ in range(k):
        best, best_score = None, None
        for idx in remaining:
            row = pool.loc[idx]
            # Score = summed current coverage of this candidate's 5 stratum values. LOWER is
            # better: its combination of format/actor/PCA-bins is currently the rarest, so
            # picking it spreads the sample the most.
            score = sum(counts[d].get(row[d], 0) for d in SPREAD_DIMS)
            jitter = rng.random() * 1e-6          # seeded, deterministic tie-break
            if best_score is None or score + jitter < best_score:
                best, best_score = idx, score + jitter
        picked.append(best)                       # take the least-represented candidate
        remaining.remove(best)
        row = pool.loc[best]
        # Update the counters so the NEXT iteration accounts for what we just added.
        for d in SPREAD_DIMS:
            counts[d][row[d]] = counts[d].get(row[d], 0) + 1
    return picked


def strata_counts(df):
    """Per stratum dimension, a {value: count} dict for the rows in df. Used to seed
    greedy_spread with what the sample already holds so the next draw complements it."""
    return {d: df[d].value_counts().to_dict() for d in SPREAD_DIMS}


def distribute(need, weights, avail):
    """Largest-remainder split of `need` across keys by `weights`, capped by `avail`.

    Used to spread the pad-to-150 fill across genres by the sqrt-compromise shape,
    so the top-up does not pile into the loudest genres.
    """
    keys = list(weights.index)
    w = weights / weights.sum()                   # normalise weights to fractions summing to 1
    ideal = {k: float(w[k]) * need for k in keys} # each key's fractional ideal share of `need`
    assign = {k: min(int(np.floor(ideal[k])), int(avail[k])) for k in keys}  # floor, capped by availability
    rem = int(need - sum(assign.values()))        # units the floors left unplaced
    # Largest-remainder rule: hand the leftovers to the keys with the biggest fractional
    # part first (ties broken by the larger weight).
    order = sorted(keys, key=lambda k: (-(ideal[k] - np.floor(ideal[k])), -float(weights[k])))
    i = guard = 0
    while rem > 0 and guard < 100000:             # guard caps the loop against an unsatisfiable request
        if all(assign[k] >= avail[k] for k in keys):
            break                                 # every key is at its availability ceiling; stop
        k = order[i % len(order)]                 # cycle through keys in largest-remainder order
        if assign[k] < avail[k]:
            assign[k] += 1
            rem -= 1
        i += 1
        guard += 1
    return assign


def main():
    # ---- load + join -------------------------------------------------------
    # sampling_frame.csv (from a6) carries the strata; attach the CLIP visual-genre labels.
    frame = pd.read_csv(os.path.join(OUT_DIR, "sampling_frame.csv"))
    vis = pd.read_csv(os.path.join(OUT_DIR, "visual_clusters.csv"))
    frame = frame.merge(vis[["Post_ID", "visual_zeroshot", "visual_hdbscan"]],
                        on="Post_ID", how="left")

    # Pull the UNCAPPED public Comments total from analysis_base (the frame's
    # n_comments_scraped is capped at 15; the high-discussion layer needs the true count).
    base = pd.read_csv(os.path.join(BASE_DIR, "analysis_base.csv"), sep=";",
                       encoding="utf-8-sig")
    frame = frame.merge(base[["Post_ID", "Comments"]], on="Post_ID", how="left")
    frame["Comments"] = pd.to_numeric(frame["Comments"], errors="coerce").fillna(0).astype(int)  # non-numeric/missing -> 0

    # A CSV round-trip turns booleans into text -> coerce the flag columns back to real bools.
    for c in ["blindspot", "comment_avail", "commemoration_day", "live_series",
              "hiring", "pure_info"]:
        frame[c] = to_bool(frame[c])
    frame["visual_zeroshot"] = frame["visual_zeroshot"].fillna("unknown")   # posts CLIP could not label
    frame = frame.set_index("Post_ID", drop=True)   # index name == "Post_ID"
    N = len(frame)

    selected = {}   # Post_ID -> selection_reason

    def mark(pid, reason):
        # Record pid under `reason`, but only ever UPGRADE a post's tag: if it is already
        # selected, keep whichever reason has the higher PRIORITY (a guarantee outranks core).
        if pid not in selected or PRIORITY[reason] > PRIORITY[selected[pid]]:
            selected[pid] = reason

    def unselected(mask):
        # Rows matching `mask` that are NOT yet selected, so each layer draws fresh posts.
        return frame[mask & ~frame.index.isin(selected)]

    # ---- core allocation (sqrt-proportional, floor 6) ----------------------
    # Same sqrt-compromise shape as a6: allocate CORE_TARGET draws across genres by sqrt(n),
    # floor CORE_FLOOR each (outliers OUTLIER_FLOOR), then cap by the genre's actual size.
    gcounts = frame["functional_genre"].value_counts()
    raw = np.sqrt(gcounts.astype(float))
    alloc = np.maximum(np.round(raw / raw.sum() * CORE_TARGET), CORE_FLOOR).astype(int)
    if "outlier_atypical" in alloc.index:
        alloc["outlier_atypical"] = max(int(alloc["outlier_atypical"]), OUTLIER_FLOOR)
    alloc = alloc.clip(upper=gcounts.astype(int))

    # ---- Layer 1: genre core ----------------------------------------------
    # For each genre, pick its allocation from the not-yet-selected pool, seeded with any
    # posts already selected in that genre so the picks spread over format/actor/PCA bins.
    for genre in alloc.index:
        pool = unselected(frame["functional_genre"] == genre)
        seed = strata_counts(frame[frame.index.isin(selected) &
                                   (frame["functional_genre"] == genre)])
        for pid in greedy_spread(pool, alloc[genre], seed):
            mark(pid, "genre_core")
    core_alloc_sum = int(alloc.sum())             # total core target before overlays (for the audit)

    # ---- Layer 2: blind-spot oversample (weighted) -------------------------
    # Draw N_BLINDSPOT extra posts from the T2 no-frame pool, over-weighting the two genres
    # the core under-counts, so the "no explicit frame" set is well represented in the sample.
    bpool = unselected(frame["blindspot"])
    if len(bpool):
        w = np.where(bpool["functional_genre"].isin(BLINDSPOT_BOOST_GENRES),
                     BLINDSPOT_BOOST, 1.0)        # boosted genres get 2x the draw weight
        w = w / w.sum()                           # normalise to a probability distribution
        k = int(min(N_BLINDSPOT, len(bpool)))
        # Weighted random draw WITHOUT replacement; rng is seeded so the pick is reproducible.
        chosen = rng.choice(bpool.index.to_numpy(), size=k, replace=False, p=w)
        for pid in chosen:
            mark(pid, "blindspot")

    # ---- Layer 3: high-discussion (>=20 total comments, <=4/org) -----------
    # sort_index first so the stable Comments-desc sort breaks ties by Post_ID.
    # (Deterministic ordering = a reproducible pick of which busy posts enter the layer.)
    hpool = unselected(frame["Comments"] >= HIGHDISC_MIN_COMMENTS) \
        .sort_index().sort_values("Comments", ascending=False, kind="stable")
    org_used, n_hd = {}, 0
    # Walk most-commented first; take a post only while its org is under the per-org cap, so
    # the comment-stance layer stays cross-organisation instead of all one loud account.
    for pid, org in zip(hpool.index, hpool["Actor_Name"]):
        if n_hd >= N_HIGHDISC:
            break
        if org_used.get(org, 0) < HIGHDISC_CAP_PER_ORG:
            mark(pid, "high_discussion")
            org_used[org] = org_used.get(org, 0) + 1
            n_hd += 1

    # ---- Layer 4: coverage top-ups (only where a floor is unmet) -----------
    def sample_df():
        return frame[frame.index.isin(selected)]   # the current selection as a DataFrame

    def topup(pool, need):
        # Add up to `need` more posts from `pool` (minus what is already selected), spread
        # over the strata; tagged coverage_guarantee (top priority, so they survive trimming).
        pool = pool[~pool.index.isin(selected)]
        if need <= 0 or len(pool) == 0:
            return
        for pid in greedy_spread(pool, min(need, len(pool))):
            mark(pid, "coverage_guarantee")

    # new-genre keyword floors: bring each rare keyword genre up to its minimum count.
    for flag, floor in NEW_GENRE_FLOORS.items():
        have = int(sample_df()[flag].sum())        # how many are already in the sample
        topup(frame[frame[flag]], floor - have)    # draw the shortfall (no-op if already met)

    # visual-genre floors: every CLIP visual genre reaches VISUAL_FLOORS (or the default).
    for vg in frame["visual_zeroshot"].dropna().unique():
        if vg == "unknown":
            continue                               # skip the "CLIP could not label" bucket
        floor = VISUAL_FLOORS.get(vg, VISUAL_FLOOR_DEFAULT)
        have = int((sample_df()["visual_zeroshot"] == vg).sum())
        topup(frame[frame["visual_zeroshot"] == vg], floor - have)

    # engagement extremes for the top-5 genres by corpus size: guarantee the single highest-
    # and lowest-engagement post of each is present, so the sample captures the tails.
    for genre in gcounts.head(ENG_EXTREME_TOP_GENRES).index:
        g = frame[frame["functional_genre"] == genre]
        for pid in [g["eng_rate"].idxmax(), g["eng_rate"].idxmin()]:   # ids of the max- and min-engagement post
            if pid not in selected:
                mark(pid, "coverage_guarantee")

    # every organisation represented at least once (representativeness)
    for org in sorted(frame["Actor_Name"].dropna().unique()):
        if int((sample_df()["Actor_Name"] == org).sum()) < 1:
            topup(frame[frame["Actor_Name"] == org], 1)   # pull one post from any missing org

    # ---- reconcile to EXACTLY 150 -----------------------------------------
    def cur():
        return frame[frame.index.isin(selected)].copy()   # a fresh snapshot of the selection

    # pad with extra core, distributed across genres by the sqrt-compromise shape
    # (so the fill does not pile into the loudest genres)
    if len(selected) < N_TOTAL:
        need = N_TOTAL - len(selected)             # how many short of 150 we are
        avail = {g: len(unselected(frame["functional_genre"] == g)) for g in gcounts.index}  # spare posts per genre
        weights = np.sqrt(gcounts.astype(float))   # same sqrt weighting as the core
        for genre, k in distribute(need, weights, avail).items():
            if k <= 0:
                continue
            pool = unselected(frame["functional_genre"] == genre)
            seed = strata_counts(cur()[cur()["functional_genre"] == genre])
            for pid in greedy_spread(pool, k, seed):
                mark(pid, "genre_core")

    # trim the most over-represented removable core posts
    if len(selected) > N_TOTAL:
        def removable(pid, samp):
            # A core post is removable ONLY if dropping it breaks no coverage guarantee: its
            # genre, format, each PCA bin, its visual floor, any new-genre floor, and its org
            # must all still be covered by other posts after it is gone.
            r = frame.loc[pid]
            g = samp[samp["functional_genre"] == r["functional_genre"]]
            if len(g) <= 1:
                return False                       # last post of its genre -> keep
            if (samp["Post_Format"] == r["Post_Format"]).sum() <= 1:
                return False                       # last of its format -> keep
            for b in ["pca1_bin", "pca2_bin", "pca3_bin"]:
                if (samp[b] == r[b]).sum() <= 1:
                    return False                   # last in a PCA tercile -> keep
            vg = r["visual_zeroshot"]
            if (samp["visual_zeroshot"] == vg).sum() <= VISUAL_FLOORS.get(vg, VISUAL_FLOOR_DEFAULT):
                return False                       # would fall below the visual-genre floor -> keep
            for flag, floor in NEW_GENRE_FLOORS.items():
                if r[flag] and int(samp[flag].sum()) <= floor:
                    return False                   # would fall below a new-genre floor -> keep
            if (samp["Actor_Name"] == r["Actor_Name"]).sum() <= 1:
                return False                       # last post from its org -> keep
            return True

        # Remove one removable post at a time, always the most over-represented, until N=150.
        while len(selected) > N_TOTAL:
            samp = cur()
            core_ids = [p for p, why in selected.items() if why == "genre_core"]   # only ever trim plain core
            # most over-represented first (largest summed strata counts)
            sc = strata_counts(samp)
            scored = sorted(
                (p for p in core_ids if removable(p, samp)),
                key=lambda p: -sum(sc[d].get(frame.loc[p, d], 0) for d in SPREAD_DIMS))
            if not scored:
                break                              # nothing safely removable -> stop (may end slightly over)
            del selected[scored[0]]                # drop the single most redundant core post

    # ---- assemble output ---------------------------------------------------
    out = frame[frame.index.isin(selected)].copy()
    out["Post_ID"] = out.index                     # lift the Post_ID index back into a column
    out = out.reset_index(drop=True)
    out["selection_reason"] = out["Post_ID"].map(selected)   # why each post was drawn (its priority tag)
    out["image_path"] = out["Post_ID"].apply(lambda p: os.path.join(MEDIA_REL, f"{p}.jpg"))   # relative jpg path
    out["image_exists"] = out["Post_ID"].apply(
        lambda p: os.path.exists(os.path.join(BASE_DIR, MEDIA_REL, f"{p}.jpg")))   # is the image file actually on disk

    cols = ["Post_ID", "Actor_Name", "Post_Format", "BERTopic", "functional_genre",
            "blindspot", "comment_avail", "n_comments_scraped", "Comments", "eng_rate",
            "pca1_bin", "pca2_bin", "pca3_bin",
            "commemoration_day", "live_series", "hiring", "pure_info",
            "visual_zeroshot", "visual_hdbscan", "selection_reason",
            "image_path", "image_exists"]
    # Sort by draw layer (core -> blindspot -> high_discussion -> coverage), then genre, then
    # id, so the CSV reads in a stable, human-scannable order (the key maps reason -> rank).
    reason_order = {"genre_core": 0, "blindspot": 1, "high_discussion": 2, "coverage_guarantee": 3}
    out = out.sort_values(by=["selection_reason", "functional_genre", "Post_ID"],
                          key=lambda s: s.map(reason_order) if s.name == "selection_reason" else s)
    out[cols].to_csv(os.path.join(BASE_DIR, "sample_150.csv"), index=False)

    # ---- coverage audit ----------------------------------------------------
    # Everything below is read-only reporting: it prints how well the drawn 150 cover each
    # stratum (genre, format, org, PCA terciles, keyword/visual floors) so the stratification
    # can be checked at a glance. No selection happens here.
    line = "=" * 70
    print(f"\n{line}\nSAMPLE DRAW AUDIT  (seed={SEED}, target N={N_TOTAL})\n{line}")
    print(f"corpus = {N} coded posts | drawn = {len(out)} "
          f"(unique = {out['Post_ID'].nunique()})")
    miss = int((~out["image_exists"]).sum())       # how many drawn posts are missing their jpg
    print(f"images present = {int(out['image_exists'].sum())}/{len(out)}"
          + (f"  MISSING {miss}" if miss else "  (all present)"))

    print(f"\n-- selection_reason (draw layer; priority tag when overlapping) --")
    print(f"   core allocation summed to {core_alloc_sum} before overlays/reconcile")
    print(out["selection_reason"].value_counts().to_string())

    print(f"\n-- functional genre  (core allocation -> drawn) --  {out['functional_genre'].nunique()}/13 genres")
    got = out["functional_genre"].value_counts()
    for genre in gcounts.index:
        print(f"   {genre:36s} alloc {int(alloc.get(genre,0)):2d} -> drawn {int(got.get(genre,0)):2d}"
              f"   (corpus {int(gcounts[genre])})")

    print(f"\n-- Post_Format --")
    print(out["Post_Format"].value_counts().to_string())

    print(f"\n-- organisations represented: {out['Actor_Name'].nunique()}/{frame['Actor_Name'].nunique()} --")
    org = out["Actor_Name"].value_counts()
    print("   " + " | ".join(f"{k} {v}" for k, v in org.items()))
    zero = sorted(set(frame["Actor_Name"]) - set(out["Actor_Name"]))
    if zero:
        print("   NOT represented: " + ", ".join(zero))

    print(f"\n-- PCA frame-space terciles (each should cover lo/mid/hi) --")
    for b in ["pca1_bin", "pca2_bin", "pca3_bin"]:
        # reindex forces the lo/mid/hi order (and fills 0 for any tercile the sample missed).
        vc = out[b].value_counts().reindex(["lo", "mid", "hi"]).fillna(0).astype(int)
        print(f"   {b}:  lo {vc['lo']:2d}  mid {vc['mid']:2d}  hi {vc['hi']:2d}")

    print(f"\n-- new-genre keyword floors --")
    for flag, floor in NEW_GENRE_FLOORS.items():
        have = int(out[flag].sum())
        print(f"   {flag:20s} {have:2d}  (floor {floor})  {'OK' if have >= floor else 'UNMET'}")

    print(f"\n-- visual genre floors (default {VISUAL_FLOOR_DEFAULT}) --")
    for vg, cnt in out["visual_zeroshot"].value_counts().items():
        floor = VISUAL_FLOORS.get(vg, VISUAL_FLOOR_DEFAULT)
        print(f"   {vg:20s} {int(cnt):2d}  (floor {floor})  {'OK' if cnt >= floor else 'UNMET'}")

    print(f"\n-- comment / stance material --")
    print(f"   posts with >=3 scraped comments (stance-eligible): "
          f"{int(out['comment_avail'].sum())}")
    print(f"   high-discussion posts (>=20 total comments):       "
          f"{int((out['Comments'] >= HIGHDISC_MIN_COMMENTS).sum())}")
    print(f"   blind-spot (T2 no-frame) posts in sample:          "
          f"{int(out['blindspot'].sum())}")

    print(f"\n-- engagement extremes present for top-5 genres --")
    # Confirm the Layer-4 guarantee held: for each top-5 genre, is its highest- and lowest-
    # engagement post actually in the sample? (Y/n per extreme.)
    for genre in gcounts.head(ENG_EXTREME_TOP_GENRES).index:
        g_all = frame[frame["functional_genre"] == genre]
        hi_id = g_all["eng_rate"].idxmax()
        lo_id = g_all["eng_rate"].idxmin()
        print(f"   {genre:36s} hi:{'Y' if hi_id in selected else 'n'}  lo:{'Y' if lo_id in selected else 'n'}")

    print(f"\nWrote sample_150.csv  ({len(out)} rows).  No Phase B tokens spent.\n{line}")


if __name__ == "__main__":
    main()