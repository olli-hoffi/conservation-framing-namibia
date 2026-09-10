#!/usr/bin/env python3
"""
Study 1 NLP Analysis Pipeline  v2.0
MSc Thesis: Message Framing on Instagram — Oliver Hoffmann, Goethe University Frankfurt

── REPRODUCIBILITY ────────────────────────────────────────────────────────────
Identical results across runs are guaranteed by:
  1. Fixed random_state=RANDOM_SEED in UMAP
  2. UMAP n_jobs=1 (single-threaded; multi-threaded UMAP cannot guarantee exact
     reproduction due to race conditions, per McInnes et al. 2018 documentation)
  3. Embedding cache: embeddings are saved to disk on first run and reloaded on
     subsequent runs — avoids any floating-point variance from re-encoding.
  4. run_metadata.json logs all package versions, parameters, and timestamps.

── WHAT THIS SCRIPT DOES ──────────────────────────────────────────────────────
Phase 1  BERTopic inductive topic clustering
         Embeds every caption once, then discovers latent topics with
         UMAP + HDBSCAN + c-TF-IDF (Grootendorst, 2022). Uses cleaned captions
         for topic labelling and raw captions for the embeddings. The cluster
         id it assigns to each post is the pipeline's substantive output.

Phase 2  Validation sample
         Stratified 20% draw (N = 184), one fifth of each organization's
         captioned posts under a fixed seed, written as a blank worksheet for
         the independent human coder behind the blind reliability standard.

── USAGE ───────────────────────────────────────────────────────────────────────
  pip install sentence-transformers bertopic umap-learn hdbscan langdetect scikit-learn
  python3 study1_nlp_pipeline.py

── WHERE THE OUTPUT GOES ───────────────────────────────────────────────────────
The cluster id of Phase 1 becomes the `BERTopic` column of
`data/study1/analysis_base.csv`, which `a6_sampling_frame.py` maps to the 13
topic categories. Those are the stratification axis of the 150-post exploratory
subsample and the scheme behind the per-organization content mix. The worksheet
of Phase 2 became the 184-post blind coding that supplies the reliability
standard of the Study 1 codebook.

── OUTPUTS ─────────────────────────────────────────────────────────────────────
  NLP_Analysis/Study1_NLP_Clusters_YYYYMMDD_HHMM.csv         per-post cluster id
  NLP_Analysis/Study1_NLP_Topics_YYYYMMDD_HHMM.csv           topic table
  NLP_Analysis/Study1_NLP_ValidationSample_YYYYMMDD_HHMM.csv blind worksheet
  NLP_Analysis/embeddings_cache_{model}_{n}.npy              embedding cache
  NLP_Analysis/run_metadata_YYYYMMDD_HHMM.json               reproducibility log
"""
#
# Status: runs offline. It needs the embedding and topic-model packages of
#         python/requirements.txt (sentence-transformers, bertopic, umap-learn,
#         hdbscan) and the embedding cache shipped in NLP_Analysis/, which makes
#         the clustering bit-for-bit reproducible without re-encoding.
#

# Standard-library helpers (ship with Python, nothing to install):
import sys            # sys.version -> record the exact Python used, for the reproducibility log
import re             # regular expressions: pattern-based text cleaning (strip URLs, @mentions, #)
import json           # write the run-metadata sidecar file
import random         # Python's own RNG; seeded below so any sampling is reproducible
import unicodedata    # inspect each character's Unicode category (used to drop emoji/symbols)
import warnings
warnings.filterwarnings("ignore")   # silence the many deprecation/convergence notes the ML libs emit,
                                    # so the console log stays readable. Does not affect results.

# Third-party scientific stack:
import pandas as pd            # data frames: the whole corpus lives in one df, one row per post
import numpy as np            # numeric arrays + the score matrices (cosine similarities live here)
from pathlib import Path       # object-oriented file paths (cross-platform, avoids string-joining paths)
from datetime import datetime  # timestamp every output file so reruns never overwrite each other
from collections import Counter # quick frequency tallies (e.g. how many posts per predicted category)

# ── PATHS ──────────────────────────────────────────────────────────────────────
# Every path is derived from this file's own location, never hard-coded, so the
# pipeline runs wherever the package sits on disk. The RAW corpus is resolved
# through python/_paths.py, the single place the package keeps its data layout.
SCRIPT_DIR  = Path(__file__).resolve().parent
_PKG_PYTHON = SCRIPT_DIR.parent.parent          # → package "python/" folder
sys.path.insert(0, str(_PKG_PYTHON))
from _paths import RAW_CORPUS                   # noqa: E402  (path set up above)

# The RAW corpus, 920 posts from the Apify scrape and the convert step.
DATA_FILE   = RAW_CORPUS
# Every output (embedding cache, per-run CSVs, metadata JSON) is written here.
OUTPUT_DIR  = SCRIPT_DIR / "NLP_Analysis"

# ── CONFIGURATION ──────────────────────────────────────────────────────────────

# Sentence-transformer model.
# all-MiniLM-L6-v2: fast, strong English performance (~80MB).
# Switch to paraphrase-multilingual-mpnet-base-v2 if multilingual captions detected.
EMBEDDING_MODEL = "all-MiniLM-L6-v2"

# BERTopic settings (Phase 1). These control how the inductive clustering behaves:
BERTOPIC_MIN_CLUSTER_SIZE = 8   # a cluster needs >= 8 posts to count as a topic (smaller = noise)
BERTOPIC_MIN_SAMPLES      = 3   # HDBSCAN conservativeness: higher = more posts left as outliers
BERTOPIC_N_NEIGHBORS      = 10  # UMAP locality: how many neighbours define each post's neighbourhood

# Reproducibility — do not change between runs if you want identical results.
# Seeding both RNGs pins every stochastic step (sampling, UMAP init) to the same
# starting point, so a rerun reproduces the exact same numbers. 42 is arbitrary.
RANDOM_SEED = 42
random.seed(RANDOM_SEED)      # seed Python's RNG (used by DataFrame.sample in Phase 2)
np.random.seed(RANDOM_SEED)   # seed NumPy's RNG (used under the hood by UMAP)


# ── TEXT CLEANING ──────────────────────────────────────────────────────────────

def clean_for_labels(text: str) -> str:
    """
    Lightly clean caption text for the BERTopic c-TF-IDF keyword labels.
    NOT used for generating embeddings — embeddings use the raw caption.

    Why two text versions?
    Transformer sentence embeddings (Reimers & Gurevych, 2019) work best on full,
    natural language — hashtags and emojis carry semantic meaning and should be
    preserved for encoding. But for the human-readable topic keywords BERTopic
    prints as a cluster label, "#rhino" and "rhino" are redundant, and emojis
    clutter output. Raw captions → encoder; cleaned captions → topic labels.
    (Preprocessing rationale: Denny & Spirling, 2018; Grootendorst, 2022.)
    """
    if not text:
        return ""
    # Remove hyperlinks — URLs add no topical content for labelling
    text = re.sub(r"http\S+|www\.\S+", "", text)
    # Remove @mentions — handle names, not substantive content
    text = re.sub(r"@\w+", "", text)
    # Hashtags: strip the # but keep the word — "wildlifeconservation" is a
    # useful topic keyword; "#wildlifeconservation" has a spurious prefix token
    text = re.sub(r"#(\w+)", r"\1", text)
    # Remove emoji and miscellaneous symbols.
    # Unicode "So" = other symbol (most emoji); "Cs" = surrogate; "Cn" = unassigned.
    text = "".join(
        c for c in text
        if not unicodedata.category(c).startswith(("So", "Cs", "Cn"))
    )
    # Collapse spaces/newlines left by removed tokens
    text = re.sub(r"\s+", " ", text).strip()
    return text


# ── DATA LOADING ──────────────────────────────────────────────────────────────

def load_data():
    """
    Read the RAW post CSV and derive the three caption columns the pipeline needs:
    caption_raw (for embeddings), caption_labels (cleaned, for topic labels), and
    has_caption (the mask marking which posts are long enough to analyse).
    """
    print("Loading data...")
    # sep=";" because the export is semicolon-delimited (German-locale Excel default);
    # utf-8-sig strips the byte-order-mark Excel prepends, so the first column name is clean.
    df = pd.read_csv(DATA_FILE, sep=";", encoding="utf-8-sig")
    # Missing captions come in as NaN; turn them into empty strings so string ops below never crash.
    df["Caption_Text"] = df["Caption_Text"].fillna("")
    # caption_raw = the natural caption (emoji/hashtags kept) -> fed to the embedding model.
    df["caption_raw"]    = df["Caption_Text"].str.strip()
    # caption_labels = the cleaned caption (URLs/emoji/# stripped) -> used only for topic keywords.
    df["caption_labels"] = df["caption_raw"].apply(clean_for_labels)
    # has_caption = the analysis mask: keep only posts with > 10 characters. Very short or
    # empty captions carry no reliable semantic signal and would distort the embeddings.
    df["has_caption"]    = df["caption_raw"].str.len() > 10

    n_empty = (~df["has_caption"]).sum()   # ~ = logical NOT; count the posts that fail the mask
    print(f"  {len(df)} posts from {df['Actor_Name'].nunique()} organizations")  # nunique = distinct orgs
    print(f"  {n_empty} posts with empty/very short captions (excluded from NLP)")
    return df


# ── LANGUAGE DETECTION ─────────────────────────────────────────────────────────

def detect_languages(df):
    """
    Tag each caption with its most likely language. Purely descriptive: it lets the
    thesis report the English/Afrikaans/German mix, and flags whether a multilingual
    embedding model would be warranted (see EMBEDDING_MODEL note above).
    """
    print("\nDetecting caption language...")
    # langdetect is optional. If it is not installed, degrade gracefully (mark everything
    # "unknown" and carry on) rather than crashing the whole pipeline over a diagnostic.
    try:
        from langdetect import detect, LangDetectException
    except ImportError:
        print("  langdetect not installed — skipping")
        df["Language_detected"] = "unknown"
        return df

    # Translate langdetect's ISO codes into readable labels for the four languages we expect.
    lang_map = {"en": "English", "af": "Afrikaans", "de": "German", "fr": "French"}

    def safe_detect(text):
        # Skip empties and very short strings: langdetect is unreliable under ~15 chars.
        if not text or len(text) < 15:
            return "unknown"
        try:
            code = detect(text)                       # returns an ISO code, e.g. "en"
            # Known code -> friendly name; anything else kept verbatim as "other:xx" so it is visible.
            return lang_map.get(code, f"other:{code}")
        except LangDetectException:
            return "unknown"                          # detector could not decide (e.g. only emoji/numbers)

    # Run the detector on every raw caption and store the label.
    df["Language_detected"] = df["caption_raw"].apply(safe_detect)
    # Print a language breakdown, most common first, as counts and % of all posts.
    for lang, count in Counter(df["Language_detected"]).most_common():
        print(f"    {lang:<20}  {count:4d}  ({count/len(df)*100:.1f}%)")
    return df


# ── EMBEDDINGS (CACHED) ────────────────────────────────────────────────────────
# What is an "embedding"? A neural language model reads each caption and returns a
# list of 384 numbers (a vector) that encodes its MEANING, not its exact words. Texts
# with similar meaning land near each other in this 384-dimensional space, so we can
# measure "how alike are these two captions" by geometry alone. The clustering below
# is built on top of these vectors.


def _cache_path(n_rows: int) -> Path:
    """Build the on-disk name for the embedding cache, keyed by model + number of rows."""
    # Sanitize the model name into a filesystem-safe slug (no "/" or "-").
    model_slug = EMBEDDING_MODEL.replace("/", "_").replace("-", "_")
    # Encoding n_rows in the filename means a different corpus size gets its own cache,
    # so we never silently reuse embeddings that belong to a different set of posts.
    return OUTPUT_DIR / f"embeddings_cache_{model_slug}_{n_rows}.npy"


def generate_embeddings(df):
    """
    Embed captions using sentence-transformers (Reimers & Gurevych, 2019).
    Results are cached to disk. Subsequent runs load the cache instead of
    re-encoding, guaranteeing bit-for-bit identical embeddings.
    Raw captions are used (not cleaned) per BERTopic documentation.
    """
    from sentence_transformers import SentenceTransformer

    mask     = df["has_caption"].values          # boolean array: True for posts we will embed
    captions = df.loc[mask, "caption_raw"].tolist()   # the raw captions for exactly those posts
    n_rows   = len(captions)                      # how many captions we are encoding this run
    cache    = _cache_path(n_rows)               # where the vectors for this corpus size live

    # Fast path: if a matching cache already exists, load the saved vectors instead of
    # re-encoding. This both saves minutes and guarantees identical numbers across runs.
    if cache.exists():
        print(f"\nLoading cached embeddings from {cache.name}")
        embeddings = np.load(cache)              # (n_rows, 384) float array from disk
        print(f"  Shape: {embeddings.shape}")
    else:
        print(f"\nGenerating sentence embeddings (model: {EMBEDDING_MODEL})...")
        print("  First run downloads ~80MB model weights.")
        model      = SentenceTransformer(EMBEDDING_MODEL)   # download/load the pretrained encoder
        # encode() turns the list of caption strings into a (n_rows, 384) matrix of vectors.
        embeddings = model.encode(
            captions,
            batch_size=64,                # encode 64 captions at a time (a speed/memory trade-off, not a result-changer)
            show_progress_bar=True,
            # normalize_embeddings=True scales each vector to unit length, which
            # makes cosine similarity equivalent to the dot product and is what
            # UMAP's cosine metric expects downstream.
            normalize_embeddings=True,
        )
        # Save to disk so subsequent runs load identical vectors rather than
        # re-computing (GPU/CPU floating-point may vary slightly between runs).
        np.save(cache, embeddings)
        print(f"  Saved to cache: {cache.name}")
        print(f"  Shape: {embeddings.shape}")

    return embeddings, mask


# ── PHASE 1: BERTOPIC ─────────────────────────────────────────────────────────

def run_bertopic(df, embeddings, mask):
    """
    BERTopic inductive topic clustering (Grootendorst, 2022).
    Uses UMAP (McInnes et al., 2018) + HDBSCAN (Campello et al., 2013) + c-TF-IDF.
    n_jobs=1 ensures deterministic UMAP output (McInnes et al. 2018, §6).
    Cleaned captions used for c-TF-IDF topic representation;
    raw embeddings used for clustering.
    """
    from bertopic import BERTopic
    from umap import UMAP
    from hdbscan import HDBSCAN
    from sklearn.feature_extraction.text import CountVectorizer

    print("\nPhase 1: BERTopic inductive topic clustering")

    captions_for_labels = df.loc[mask, "caption_labels"].tolist()

    # UMAP compresses 384-dim embeddings to 5 dims while preserving local structure.
    # n_neighbors=10: each point is positioned relative to its 10 nearest neighbors.
    #   Lower = more local clusters; higher = more global structure. 10 is appropriate
    #   for a corpus of ~900 short texts (BERTopic documentation recommendation).
    # min_dist=0.0: allows tight clusters to form (important for HDBSCAN downstream).
    # n_jobs=1: forces single-threaded execution for exact reproducibility.
    #   Multi-threaded UMAP uses parallel gradient updates that can produce slightly
    #   different layouts between runs even with the same random seed (McInnes et al. 2018).
    umap_model = UMAP(
        n_neighbors=BERTOPIC_N_NEIGHBORS,
        n_components=5,
        min_dist=0.0,
        metric="cosine",
        random_state=RANDOM_SEED,
        n_jobs=1,
    )
    # HDBSCAN finds dense regions in the 5-dim UMAP space and labels them as topics.
    # min_cluster_size=8: a topic must contain at least 8 posts to be named.
    #   Smaller = more topics, some possibly too small to interpret meaningfully.
    # Posts that don't fit any dense region are assigned to cluster -1 (outliers).
    #   These are typically cross-topic, very short, or idiosyncratic posts.
    #   For 920 captions, ~100 outliers (11%) is a typical and acceptable rate.
    hdbscan_model = HDBSCAN(
        min_cluster_size=BERTOPIC_MIN_CLUSTER_SIZE,
        min_samples=BERTOPIC_MIN_SAMPLES,
        metric="euclidean",
        cluster_selection_method="eom",
        prediction_data=True,
    )
    # This CountVectorizer does NOT cluster; it only builds the word counts BERTopic
    # turns into each cluster's keyword label (via c-TF-IDF): keep 1-2 word terms,
    # drop English stopwords, ultra-rare and ultra-common ones.
    vectorizer = CountVectorizer(
        ngram_range=(1, 2),
        stop_words="english",
        min_df=2,
        max_df=0.80,
    )

    # Assemble the three sub-models into one BERTopic pipeline:
    #   embeddings -> UMAP (reduce) -> HDBSCAN (cluster) -> CountVectorizer/c-TF-IDF (label).
    topic_model = BERTopic(
        umap_model=umap_model,
        hdbscan_model=hdbscan_model,
        vectorizer_model=vectorizer,
        nr_topics="auto",     # let BERTopic merge very similar clusters automatically (no fixed k)
        verbose=False,
    )

    print("  Fitting BERTopic on cached embeddings...")
    # Pass the cleaned captions (for keyword labelling) AND the precomputed embeddings
    # (for clustering) - so we reuse the exact cached vectors rather than re-encoding.
    topics, _ = topic_model.fit_transform(captions_for_labels, embeddings)

    topic_info = topic_model.get_topic_info()   # one row per topic + one row for the outlier cluster (-1)
    n_topics   = len(topic_info) - 1            # subtract that outlier row to get the real topic count
    n_outliers = sum(1 for t in topics if t == -1)   # posts HDBSCAN could not confidently place
    print(f"  {n_topics} topics found  |  {n_outliers} outlier posts")

    # Map cluster assignments back onto the full 920-row frame; -99 sentinel = post had no caption.
    cluster_col = np.full(len(df), -99, dtype=int)
    cluster_col[mask] = topics

    return topic_model, cluster_col, topic_info


# ── PHASE 2: VALIDATION SAMPLE ────────────────────────────────────────────────

def generate_validation_sample(df, sample_pct=0.20):
    """
    Build the blind-coding worksheet (N = 184). Every MANUAL_* column is left BLANK and
    the sheet carries no machine codes at all, so the human coder judges each post from
    the caption and the post itself. That is what makes the resulting codes an
    independent standard rather than a review of the automated pass.
    IMPORTANT: the MANUAL_* cells are the human ground truth and must be coded by a
    person, never auto-filled, or the reliability check would be circular.
    """
    print(f"\nPhase 2: Stratified validation sample ({sample_pct*100:.0f}%)")

    base = df[df["has_caption"]].copy()   # only captioned posts are eligible for the NLP-vs-manual check
    sample_idx = []
    # Stratify by organization: draw ~20% from EACH actor separately so every org is
    # represented in proportion to its output (a simple random 20% could miss small orgs).
    for _, group in base.groupby("Actor_Name"):
        n = max(1, round(len(group) * sample_pct))   # at least 1 post per org, else 20% rounded
        # random_state=RANDOM_SEED makes the draw reproducible; min(n, len) guards tiny groups.
        sample_idx.extend(group.sample(n=min(n, len(group)), random_state=RANDOM_SEED).index)

    # Pull the human-readable context columns the coder needs to judge each post.
    sample_df = df.loc[
        sample_idx,
        ["Post_ID", "Post_URL", "Actor_Name", "Actor_Type", "Posting_Date",
         "Post_Format", "Likes", "Comments", "Caption_Text"],
    ].copy()

    # The blank MANUAL_ columns the human fills in. The list spans all 19 codebook
    # variables, the visual ones included (Youth_Style, Protagonist, Onscreen_Text,
    # Data_Visual), because the coder assesses the actual media and not just the caption.
    for var in ["Language", "Primary_Topic", "Empathy", "Threat", "Efficacy",
                "Collective_ID", "Normative", "Moral", "Economic", "Scientific",
                "Primary_CTA", "Youth_Addressed", "Youth_Style", "Interactivity",
                "Protagonist", "Emotional_Valence", "Story_Structure",
                "Onscreen_Text", "Data_Visual"]:
        sample_df[f"MANUAL_{var}"] = ""   # blank on purpose - the coder enters these by hand

    print(f"  {len(sample_df)} posts across {df.loc[sample_idx, 'Actor_Name'].nunique()} orgs")
    return sample_df


# ── RUN METADATA ──────────────────────────────────────────────────────────────

def save_run_metadata(ts: str):
    """
    Write a JSON sidecar recording exactly how this run was configured: Python and
    package versions, the random seed, model name, and the BERTopic/UMAP
    settings. This is the reproducibility receipt - anyone rerunning the pipeline can
    check they are on the same versions, since results are only guaranteed to match
    when the package versions (and cache file) match.
    """
    import importlib.metadata as ilm   # standard-library way to read an installed package's version
    packages = {}
    # Record the installed version of every library the pipeline depends on.
    for pkg in ["sentence-transformers", "bertopic", "umap-learn", "hdbscan",
                "scikit-learn", "langdetect", "numpy", "pandas", "torch"]:
        try:
            packages[pkg] = ilm.version(pkg)
        except ilm.PackageNotFoundError:
            packages[pkg] = "not installed"   # note absence rather than crashing (e.g. optional langdetect)

    meta = {
        "run_timestamp":    ts,
        "python_version":   sys.version,
        "random_seed":      RANDOM_SEED,
        "embedding_model":  EMBEDDING_MODEL,
        "bertopic": {
            "min_cluster_size": BERTOPIC_MIN_CLUSTER_SIZE,
            "min_samples":      BERTOPIC_MIN_SAMPLES,
            "n_neighbors":      BERTOPIC_N_NEIGHBORS,
        },
        "umap_n_jobs":      1,
        "umap_random_state": RANDOM_SEED,
        "packages":         packages,
        "data_file":        str(DATA_FILE),
        "notes": (
            "Embeddings are cached; reload from cache on subsequent runs. "
            "UMAP n_jobs=1 ensures deterministic output (McInnes et al. 2018). "
            "Identical results guaranteed if same cache file and same package versions are used."
        ),
    }

    path = OUTPUT_DIR / f"run_metadata_{ts}.json"
    with open(path, "w") as f:
        json.dump(meta, f, indent=2)   # indent=2 -> human-readable, pretty-printed JSON
    print(f"Saved: {path.name}")
    return meta


# ── MAIN ──────────────────────────────────────────────────────────────────────

def main():
    """Run the whole pipeline end to end: load -> embed -> Phases A-E -> save + summarise."""
    # One timestamp for the entire run; stamped into every output filename so reruns
    # never clobber earlier results (they sit side by side, dated).
    ts_str = datetime.now().strftime("%Y-%m-%d_%H%M")
    print("=" * 68)
    print("Study 1 NLP Analysis Pipeline v2.0 — Message Framing on Instagram MSc Thesis")
    print(f"Run: {datetime.now().strftime('%Y-%m-%d %H:%M')}  |  Seed: {RANDOM_SEED}")
    print("=" * 68)

    OUTPUT_DIR.mkdir(exist_ok=True)   # create NLP_Analysis/ if missing; exist_ok = no error if it is already there

    # Load
    df = load_data()
    df = detect_languages(df)

    # Embed (cached)
    embeddings, mask = generate_embeddings(df)

    # Phase 1: BERTopic
    topic_model, cluster_col, topic_info = run_bertopic(df, embeddings, mask)

    # Phase 2: Validation sample
    val_df = generate_validation_sample(df)

    # Assemble the per-post output: the identifier/metadata columns plus the cluster id.
    base_cols = ["Post_ID", "Post_URL", "Actor_ID", "Actor_Name", "Actor_Type",
                 "Instagram_Handle", "Posting_Date", "Post_Format",
                 "Likes", "Comments", "Caption_Text", "Language_detected"]
    output_df = df[base_cols].copy()
    output_df["BERTopic_cluster"] = cluster_col

    # Save outputs (semicolon-delimited + utf-8-sig so they reopen cleanly in Excel).
    clusters_path = OUTPUT_DIR / f"Study1_NLP_Clusters_{ts_str}.csv"
    output_df.to_csv(clusters_path, index=False, sep=";", encoding="utf-8-sig")   # index=False: no extra row
    print(f"\nSaved: {clusters_path.name}")

    # BERTopic topic table (topic id, size, keywords) - default comma CSV.
    topics_path = OUTPUT_DIR / f"Study1_NLP_Topics_{ts_str}.csv"
    topic_info.to_csv(topics_path, index=False)
    print(f"Saved: {topics_path.name}")

    # The blind-coding worksheet, MANUAL_ columns empty for the human coder.
    val_path = OUTPUT_DIR / f"Study1_NLP_ValidationSample_{ts_str}.csv"
    val_df.to_csv(val_path, index=False, sep=";", encoding="utf-8-sig")
    print(f"Saved: {val_path.name}  ({len(val_df)} posts)")

    save_run_metadata(ts_str)   # the reproducibility receipt (versions + settings)

    # Summary: cluster sizes, for a quick read of how the corpus split.
    print("\n" + "=" * 68)
    n_topics = int((topic_info["Topic"] >= 0).sum())
    n_noise  = int((cluster_col == -1).sum())
    print(f"BERTOPIC SUMMARY  ({n_topics} topics, {n_noise} posts left as noise)")
    print("=" * 68)
    for _, row in topic_info[topic_info["Topic"] >= 0].head(10).iterrows():
        print(f"  Topic {int(row['Topic']):>3}  {int(row['Count']):4d} posts  {str(row['Name'])[:52]}")

    print(f"\nAll outputs: {OUTPUT_DIR}")
    print("\nNEXT STEPS:")
    print("  1. Topics CSV - read the clusters and assign the descriptive category labels")
    print("  2. ValidationSample CSV - hand to the blind coder to fill the MANUAL_* columns")
    print("=" * 68)


# Standard Python entry-point guard: run main() only when the file is executed
# directly (python3 study1_nlp_pipeline.py), not when it is imported by another module.
if __name__ == "__main__":
    main()
