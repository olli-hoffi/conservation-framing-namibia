#!/usr/bin/env python3
"""
Phase C - Theme 2 (data-generation). Caption construction and craft features on
the FULL coded corpus (N=917): readability, agency / pronoun voice, imperatives
and questions, concreteness, NRC emotion, Moral Foundations (dictionary method),
emoji use, hashtag strategy, and language identification.

Dictionary-based emotion (NRC) and morality (MFD) are lexical approximations, not
gold-standard annotation; treated as exploratory descriptors. Readability uses
textstat on a cleaned caption (URLs / @ / # / emoji stripped).

Reads : analysis_base.csv, data/raw/Study1/Study1_Posts_RAW_20260520.csv
Writes: output/caption_features.csv (per-post),
        output/caption_hashtags_long.csv, output/caption_hashtag_top.csv,
        output/caption_emoji_top.csv, output/caption_screen_summary.csv,
        output/caption_screen_matches.csv
Run   : "../4. NLP Pipeline/.venv/bin/python" c3_caption_features.py
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
# WHY caption features: the caption is where framing is most explicit. This script
# turns each caption into ~50 numeric descriptors of HOW it is written - how readable,
# how directly it addresses "you", how many imperatives (calls to action), how
# emotive, how morally loaded, how many hashtags. These describe the persuasive craft
# of Namibian conservation captions and feed later exploratory comparisons in R.
import os, re, glob, ssl          # re: regex text parsing; ssl: needed to relax cert checks for the NLTK download below
from collections import Counter
import numpy as np
import pandas as pd

# one-time NLTK bootstrap (nrclex/textblob need wordnet etc.). SSL verify is
# disabled only for these NLTK downloads; no-op once the corpora are cached.
# (Some networks present certificates NLTK's downloader rejects; this unblocks the
# first fetch. After the corpora are cached locally, nothing here hits the network.)
try:
    ssl._create_default_https_context = ssl._create_unverified_context
except Exception:
    pass
import nltk
# For each required NLTK resource: check whether it is already on disk (nltk.data.find),
# and only download it if missing. Avoids re-downloading on every run.
for _r, _path in [("wordnet", "corpora/wordnet"), ("omw-1.4", "corpora/omw-1.4"),
                  ("averaged_perceptron_tagger_eng", "taggers/averaged_perceptron_tagger_eng"),
                  ("punkt_tab", "tokenizers/punkt_tab"), ("brown", "corpora/brown")]:
    try:
        nltk.data.find(_path)
    except LookupError:
        nltk.download(_r, quiet=True)

import textstat, emoji          # textstat: readability formulas; emoji: detect/strip emoji
from nrclex import NRCLex       # NRC Word-Emotion lexicon (anger/fear/joy/...)
from langdetect import detect_langs, DetectorFactory
DetectorFactory.seed = 0        # fix langdetect's RNG so language IDs are reproducible run-to-run

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
from _paths import ANALYSIS_BASE as _BASE, RAW as _RAW, FEATURES as _FEAT

UP = os.path.normpath(os.path.join(HERE, ".."))
OUT = str(_FEAT)
os.makedirs(OUT, exist_ok=True)

# ---- lexicons -----------------------------------------------------------------
# Pronoun sets grouped by grammatical person. Counting these measures the caption's
# VOICE: "we/us" = collective/organisational agency, "you/your" = direct audience
# address (a persuasion device), "they/them" = talking about third parties.
PRON = {
    "i_sing": {"i", "me", "my", "mine", "myself"},
    "we_plur": {"we", "us", "our", "ours", "ourselves"},
    "you_2nd": {"you", "your", "yours", "yourself", "yourselves"},
    "they_3rd": {"they", "them", "their", "theirs", "themselves"},
}
# Verbs that commonly open an imperative sentence ("Join us", "Protect wildlife").
# A caption starting a sentence with one of these is flagged as a call to action.
IMPERATIVE_VERBS = {
    "join", "help", "donate", "learn", "share", "support", "protect", "discover",
    "follow", "visit", "read", "watch", "save", "sign", "click", "tag", "comment",
    "download", "register", "subscribe", "explore", "meet", "see", "find", "get",
    "check", "take", "give", "stop", "stand", "act", "make", "come", "let", "do",
    "spread", "report", "adopt", "volunteer", "book", "grab", "swipe", "listen",
    "celebrate", "remember", "imagine", "consider", "vote", "pledge", "leave", "add",
}
# Moral Foundations Dictionary (MFD-style stems; '*' = prefix match). Virtue+vice
# merged per foundation. Lexical approximation for exploratory moral framing.
# Maps Haidt's five moral foundations (care, fairness, loyalty, authority, sanctity)
# to word stems; counting hits gauges which moral vocabulary a caption leans on.
MFD = {
    "care": ["safe*", "peace*", "compassion*", "empath*", "sympath*", "care", "caring",
             "protect*", "shelter", "amity", "secur*", "benefit*", "defen*", "guard*",
             "preserv*", "rescu*", "shield*", "nurtur*", "kind", "kindness", "heal*",
             "welfare", "wellbeing", "suffer*", "cruel*", "brutal*", "harm*", "hurt*",
             "kill*", "endanger*", "threat*", "danger*", "abus*", "wound*", "violence",
             "victim*", "vulnerab*", "poach*", "slaughter*", "death", "die", "dying", "orphan*"],
    "fairness": ["fair*", "fairly", "justice", "just", "equal*", "equit*", "reciproc*",
                 "impartial*", "egalitar*", "right", "rights", "balance*", "unfair*",
                 "injust*", "bias*", "discriminat*", "prejudic*", "exclud*", "unequal",
                 "corrupt*", "cheat*", "exploit*", "dishonest*", "share fairly", "deserv*"],
    "loyalty": ["loyal*", "solidarity", "patriot*", "communit*", "together", "unite*",
                "unity", "collective", "member*", "team", "family", "belong*", "nation*",
                "ally", "allies", "partner*", "fellow*", "group", "tribe", "clan",
                "betray*", "traitor*", "disloyal*", "abandon*", "individual*", "foreign*"],
    "authority": ["authorit*", "obey*", "duty", "law", "lawful*", "legal*", "order",
                  "leader*", "govern*", "ministr*", "policy", "polic*", "regulat*",
                  "permit*", "licens*", "comply*", "complian*", "rule*", "control*",
                  "official*", "institut*", "manage*", "enforce*", "protocol*",
                  "guideline*", "mandate*", "chief*", "elder*", "tradition*", "respect*",
                  "illegal*", "defy*", "rebel*", "disobey*"],
    "sanctity": ["sacred*", "holy", "pure*", "purity", "clean*", "sanctit*", "wholesome*",
                 "pristine", "natural", "nature", "wild*", "wilderness", "sanctuar*",
                 "heritage", "precious", "beauty", "beautiful", "majestic", "wonder*",
                 "miracle*", "bless*", "spirit*", "sin", "dirty", "pollut*", "contaminat*",
                 "degrad*", "waste*", "disgust*", "filth*", "desecrat*", "defile*"],
}
# Pre-split each foundation's stems into exact words vs. prefix stems (those ending
# in '*'). Doing this once up front keeps the per-word matching loop fast.
def compile_mfd(stems):
    exact, pref = set(), []
    for s in stems:
        if s.endswith("*"):
            pref.append(s[:-1])   # store the stem without the trailing '*' for startswith() matching
        else:
            exact.add(s)          # whole-word match
    return exact, pref
MFD_C = {f: compile_mfd(v) for f, v in MFD.items()}   # {foundation: (exact_set, prefix_list)}

# Regexes for stripping / detecting caption elements.
URL = re.compile(r"https?://\S+|www\.\S+|\S+\.(?:com|org|net)\b", re.I)   # links (removed before readability)
TAG = re.compile(r"#\w+")            # hashtags
MENTION = re.compile(r"@[\w.]+")     # @mentions
WORD = re.compile(r"[a-z']+")        # word tokens (lower-cased alphabetic, apostrophes kept for "don't")
NUM = re.compile(r"(?<!\w)\d+(?:[.,]\d+)?")   # standalone numbers (concreteness cue), not those glued inside a word
YEAR = re.compile(r"\b(?:19|20)\d{2}\b")      # 4-digit years 1900-2099

# Coarse emoji semantics: bucket emoji names into themes so we can describe WHAT kind
# of emoji a caption uses (animals/nature vs. faces vs. hearts vs. gestures) rather
# than just how many.
EMO_BUCKETS = {
    "nature_animal": ["animal", "elephant", "lion", "tiger", "leopard", "turtle", "whale",
                      "bird", "paw", "tree", "leaf", "plant", "flower", "globe", "earth",
                      "mountain", "desert", "sun", "water", "ocean", "wave", "fish", "frog",
                      "snake", "giraffe", "cheetah", "rhino", "penguin", "deciduous",
                      "evergreen", "cactus", "seedling", "herb", "blossom", "monkey",
                      "crocodile", "lizard", "butterfly", "bee", "hippopotamus", "zebra",
                      "camel", "bug", "spider", "shark", "octopus", "coral"],
    "face_emotion": ["face", "smiling", "grinning", "crying", "tear", "heart_eyes",
                     "pleading", "sob", "sweat", "cold_sweat"],
    "heart": ["heart", "love"],
    "hand_gesture": ["hand", "folded", "raising", "clap", "thumbs", "fist", "muscle",
                     "point", "raised", "call_me", "ok_hand", "pray"],
}
# Given an emoji's :shortcode: name, return its theme bucket (first keyword hit wins).
def emo_bucket(name):
    n = name.strip(":").lower()               # ":elephant:" -> "elephant"
    for b, keys in EMO_BUCKETS.items():
        if any(k in n for k in keys):
            return b
    return "symbol_other"                     # anything unmatched (flags, symbols, arrows...)

# ---- load captions ------------------------------------------------------------
# NOTE: no comment= filter. This NLP CSV has no '#' header line, and captions are
# hashtag-rich (503/917 posts); comment='#' would truncate every caption at its
# first hashtag. Quoted multiline caption fields are parsed by the default engine.
# Captions come from the raw corpus, every column read as string (dtype=str) so
# nothing is coerced, then a {Post_ID: caption} lookup.
capdf = pd.read_csv(os.path.join(str(_RAW), "Study1", "Study1_Posts_RAW_20260520.csv"),
                    sep=";", encoding="utf-8-sig", dtype=str)
cap = dict(zip(capdf["Post_ID"], capdf["Caption_Text"].fillna("")))   # missing captions -> empty string

base = pd.read_csv(str(_BASE), sep=";", encoding="utf-8-sig")
coded = base[base["coded"] == 1]["Post_ID"].tolist()   # the set of post IDs to process (coded corpus)

# ---- documented lower-bound caption screens ---------------------------------
# These are deliberately narrow string screens, not model codes. The rule file is
# kept beside the script so the exact list can be printed in Appendix B. A hit is
# evidence that the named material occurs; a miss is not evidence that no paraphrase
# occurs, which is why both rates are reported as lower bounds.
screen_terms = pd.read_csv(os.path.join(HERE, "caption_screen_terms.csv"))
screen_rows = []
for _, post in base.loc[base["coded"] == 1, ["Post_ID", "Actor_Name", "Handle"]].iterrows():
    text = cap.get(post["Post_ID"], "")
    if not str(text).strip():
        continue
    for screen, rules in screen_terms.groupby("screen", sort=False):
        hits = []
        phrases = []
        for _, rule in rules.iterrows():
            match = re.search(rule["regex"], text, flags=re.I)
            if match:
                hits.append(rule["rule_id"])
                phrases.append(match.group(0))
        if hits:
            screen_rows.append({
                "screen": screen,
                "Post_ID": post["Post_ID"],
                "Actor_Name": post["Actor_Name"],
                "Handle": post["Handle"],
                "matched_rules": "|".join(hits),
                "matched_phrases": "|".join(phrases),
            })

screen_matches = pd.DataFrame(screen_rows)
n_captioned = sum(bool(str(cap.get(pid, "")).strip()) for pid in coded)
screen_summary = []
for screen in screen_terms["screen"].drop_duplicates():
    matched = screen_matches[screen_matches["screen"] == screen]
    screen_summary.append({
        "screen": screen,
        "n_matched_captions": int(matched["Post_ID"].nunique()),
        "n_captioned_posts": n_captioned,
        "pct_captioned_posts": round(100 * matched["Post_ID"].nunique() / n_captioned, 1),
        "n_organizations": int(matched["Actor_Name"].nunique()),
        "interpretation": "documented lower bound",
    })
screen_summary = pd.DataFrame(screen_summary)
screen_summary.to_csv(os.path.join(OUT, "caption_screen_summary.csv"), index=False)
screen_matches.to_csv(os.path.join(OUT, "caption_screen_matches.csv"), index=False)

# ---- per-post feature extraction ----------------------------------------------
# Turn one caption string into a dict of numeric features. Returns (features, hashtags).
def features(text):
    text = text or ""                          # guard None
    tags = TAG.findall(text)                   # raw hashtags, kept from the ORIGINAL text
    mentions = MENTION.findall(text)           # raw @mentions
    # Build a "clean" caption for readability/word stats: strip URLs, tags, mentions,
    # emoji, then collapse whitespace. We keep the original `text` for emoji/hashtag
    # counts and only clean the copy used for linguistic measures.
    clean = URL.sub(" ", text)
    clean = TAG.sub(" ", MENTION.sub(" ", clean))
    clean = emoji.replace_emoji(clean, "")
    clean = re.sub(r"\s+", " ", clean).strip()
    words = WORD.findall(clean.lower())        # word tokens of the cleaned caption
    nw = len(words)                            # word count (denominator for most rates)
    f = {}

    # readability (guard very short text)
    # Flesch-Kincaid grade + Flesch reading ease + Gunning fog: standard readability
    # indices. Only computed when >=5 words (they are meaningless on a fragment) and
    # wrapped in try/except because textstat can throw on odd inputs.
    if nw >= 5:
        try:
            f["fk_grade"] = round(textstat.flesch_kincaid_grade(clean), 2)      # US school grade needed to read it
            f["flesch_ease"] = round(textstat.flesch_reading_ease(clean), 2)    # 0-100, higher = easier
            f["gunning_fog"] = round(textstat.gunning_fog(clean), 2)            # years of education needed
        except Exception:
            f["fk_grade"] = f["flesch_ease"] = f["gunning_fog"] = np.nan
    else:
        f["fk_grade"] = f["flesch_ease"] = f["gunning_fog"] = np.nan

    # agency / pronouns (per 100 words)
    # Rate per 100 words makes captions of different lengths comparable. wc counts each
    # word; per100 sums the counts of the pronouns in a set and scales to 100 words.
    wc = Counter(words)
    per100 = (lambda k: round(100 * sum(wc[w] for w in PRON[k]) / nw, 2) if nw else 0.0)
    f["pron_i"] = per100("i_sing"); f["pron_we"] = per100("we_plur")
    f["pron_you"] = per100("you_2nd"); f["pron_they"] = per100("they_3rd")
    f["direct_address"] = f["pron_you"]  # explicit 2nd-person address

    # imperatives + questions (sentence level)
    # Split the cleaned caption into sentences, then flag each sentence whose FIRST
    # word is an imperative verb (or a leading "please/let/don't/do/never" + more).
    # imperative_rate = share of sentences that are calls to action.
    sents = [s.strip() for s in re.split(r"[.!?\n]+", clean) if s.strip()]
    imp = 0
    for s in sents:
        toks = WORD.findall(s.lower())
        if not toks:
            continue
        head = toks[0]                         # the sentence-initial word
        if head in IMPERATIVE_VERBS or (head in {"please", "let", "don't", "dont", "do", "never"} and len(toks) > 1):
            imp += 1
    f["n_sentences"] = len(sents)
    f["n_imperatives"] = imp
    f["imperative_rate"] = round(imp / len(sents), 3) if sents else 0.0
    f["n_questions"] = text.count("?")         # question marks in the ORIGINAL text (dialogue/engagement cue)

    # concreteness / specificity
    # Numbers, percentages, years, currency signals = concrete/factual framing.
    # long_word_ratio + ttr describe lexical sophistication and diversity.
    nums = NUM.findall(clean)
    f["n_numbers"] = len(nums)
    f["num_per100w"] = round(100 * len(nums) / nw, 2) if nw else 0.0
    f["has_percent"] = int("%" in text or "percent" in clean.lower())
    f["has_year"] = int(bool(YEAR.search(clean)))
    f["has_currency"] = int(bool(re.search(r"N\$|\$|€|£|\bNAD\b|\bUSD\b", text)))   # Namibian dollar, USD, EUR, GBP
    longw = sum(1 for w in words if len(w) >= 7)
    f["long_word_ratio"] = round(longw / nw, 3) if nw else 0.0     # share of words >=7 chars
    f["ttr"] = round(len(set(words)) / nw, 3) if nw else 0.0  # lexical diversity (type-token ratio: unique words / total)
    f["n_words_clean"] = nw

    # NRC emotion (frequencies)
    # NRCLex maps words to eight emotions + positive/negative. affect_frequencies gives
    # each category's share of the caption's emotion words. Pre-seed all keys to 0 so
    # every post has the full column set even if no emotion word is found.
    for k in ["anger", "fear", "joy", "trust", "anticipation", "sadness", "disgust",
              "surprise", "positive", "negative"]:
        f["nrc_" + k] = 0.0
    if nw >= 3:
        try:
            n = NRCLex(); n.load_raw_text(clean)
            for k, v in (n.affect_frequencies or {}).items():
                if "nrc_" + k in f:
                    f["nrc_" + k] = round(float(v), 4)
        except Exception:
            pass

    # Moral Foundations (dictionary hits per 100 words)
    # For each foundation, count words that either match an exact term or start with a
    # prefix stem, then scale to per-100-words. mfd_total sums the five foundations.
    for found, (exact, pref) in MFD_C.items():
        hits = 0
        for w in words:
            if w in exact or any(w.startswith(p) for p in pref):
                hits += 1
        f["mfd_" + found] = round(100 * hits / nw, 2) if nw else 0.0
    f["mfd_total"] = round(sum(f["mfd_" + x] for x in MFD_C), 2)

    # emoji
    # Count emoji on the ORIGINAL text (they were stripped from `clean`). Per-100-chars
    # normalises for caption length; the bucket counts describe emoji THEME (see above).
    elist = emoji.emoji_list(text)
    echars = [e["emoji"] for e in elist]
    f["n_emoji"] = len(echars)
    f["n_emoji_unique"] = len(set(echars))
    f["emoji_per100c"] = round(100 * len(echars) / len(text), 2) if text else 0.0
    buckets = Counter(emo_bucket(emoji.demojize(c)) for c in echars)   # demojize -> :name:, then classify
    for b in list(EMO_BUCKETS) + ["symbol_other"]:
        f["emoji_" + b] = buckets.get(b, 0)

    # hashtags
    # Count total vs. unique tags, plus WHERE the first hashtag sits: a high trailing
    # ratio means tags are dumped at the end (SEO style), a low one means tags are
    # woven into the sentence.
    f["n_hashtags"] = len(tags)
    f["n_hashtags_unique"] = len(set(t.lower() for t in tags))
    f["n_mentions"] = len(mentions)
    # placement: share of caption length before the first hashtag (1 = tags trailing)
    if tags:
        first = text.find(tags[0])
        f["hashtag_trailing_ratio"] = round(first / len(text), 3) if text else 0.0
    else:
        f["hashtag_trailing_ratio"] = np.nan

    # language id
    # langdetect returns ranked language guesses; take the top one plus its probability.
    # "und" (undetermined) for captions too short to classify or on failure.
    if nw >= 3:
        try:
            best = detect_langs(clean)[0]
            f["lang"] = best.lang; f["lang_prob"] = round(best.prob, 3)
        except Exception:
            f["lang"] = "und"; f["lang_prob"] = np.nan
    else:
        f["lang"] = "und"; f["lang_prob"] = np.nan
    return f, tags

# ---- hashtag classification ---------------------------------------------------
# Sort each hashtag into a strategy class: branded (org/place tokens), campaign
# (dated/awareness-day hooks), generic (broad conservation SEO tags), or community.
ORG_TOKENS = {"wwf", "namibia", "naankuse", "ehra", "gobabeb", "ongava", "harnas",
              "ocean", "cheetah", "ccf", "nnf", "africat", "nacso", "eif", "tosco",
              "youth4can", "giraffe", "rhino", "desertlion"}
CAMPAIGN_HINT = ["day", "week", "month", "earthhour", "worldwildlife", "hour", "2025", "2026", "challenge"]
GENERIC = {"wildlife", "conservation", "nature", "wildlifephotography", "africa",
           "safari", "animals", "naturephotography", "wild", "sustainability",
           "biodiversity", "ecotourism", "photography", "travel", "namibia"}
def classify_tag(t):
    x = t.lstrip("#").lower()                          # normalise "#EarthHour" -> "earthhour"
    if any(h in x for h in CAMPAIGN_HINT):
        return "campaign"                              # dated / awareness-day tags checked first
    if any(o in x for o in ORG_TOKENS) and x not in GENERIC:
        return "branded"                               # org- or place-specific (but not if it is a generic term)
    if x in GENERIC:
        return "generic"                               # broad SEO conservation tag
    return "community_other"                           # everything else

# Run the extractor over every coded post and accumulate: per-post feature rows, a
# long hashtag table (one row per post-tag), and corpus-wide tag/emoji frequency counts.
rows, tags_long, all_tags, all_emoji = [], [], Counter(), Counter()
for pid in coded:
    f, tags = features(cap.get(pid, ""))
    f["Post_ID"] = pid
    rows.append(f)
    for t in tags:
        tl = t.lstrip("#").lower()
        tags_long.append({"Post_ID": pid, "hashtag": tl, "tag_class": classify_tag(t)})   # long format for R counting
        all_tags[tl] += 1                              # corpus hashtag frequency
    for e in emoji.emoji_list(cap.get(pid, "")):
        all_emoji[e["emoji"]] += 1                     # corpus emoji frequency

# Assemble the per-post feature matrix; move the key/length columns to the front.
feat = pd.DataFrame(rows).set_index("Post_ID").reset_index()
front = ["Post_ID", "n_words_clean", "n_sentences", "fk_grade", "flesch_ease", "gunning_fog"]
feat = feat[front + [c for c in feat.columns if c not in front]]
feat.to_csv(os.path.join(OUT, "caption_features.csv"), index=False)

# Write the long hashtag table + ranked top-hashtag and top-emoji frequency tables.
pd.DataFrame(tags_long).to_csv(os.path.join(OUT, "caption_hashtags_long.csv"), index=False)
tag_top = (pd.DataFrame([(t, c, classify_tag("#" + t)) for t, c in all_tags.most_common()],
                        columns=["hashtag", "count", "tag_class"]))   # most_common() = descending by count
tag_top.to_csv(os.path.join(OUT, "caption_hashtag_top.csv"), index=False)
emo_top = pd.DataFrame([(e, c, emo_bucket(emoji.demojize(e))) for e, c in all_emoji.most_common()],
                       columns=["emoji", "count", "bucket"])
emo_top.to_csv(os.path.join(OUT, "caption_emoji_top.csv"), index=False)

# ---- audit --------------------------------------------------------------------
# Console summary: corpus medians for the headline caption features so the extraction
# can be sanity-checked without opening the CSVs.
print(f"c3_caption_features.py: {len(feat)} posts x {feat.shape[1]-1} features -> caption_features.csv")   # -1 excludes Post_ID
print(f"  hashtags_long ({len(tags_long)} rows), hashtag_top ({len(tag_top)}), emoji_top ({len(emo_top)})")
print("\nDocumented caption screens (conservative lower bounds):")
print(screen_summary.to_string(index=False))
print("\nReadability (median): FK grade {:.1f} | Flesch ease {:.0f} | Gunning fog {:.1f}".format(
    feat.fk_grade.median(), feat.flesch_ease.median(), feat.gunning_fog.median()))
print("Voice per 100 words (median): I {:.2f} | we {:.2f} | you {:.2f} | they {:.2f}".format(
    feat.pron_i.median(), feat.pron_we.median(), feat.pron_you.median(), feat.pron_they.median()))
print("Imperatives: {:.0f}% of posts contain >=1; median rate {:.2f}".format(
    100 * (feat.n_imperatives > 0).mean(), feat.imperative_rate.median()))
print("Concreteness: {:.0f}% have a number, {:.0f}% a percent, {:.0f}% a year".format(
    100 * (feat.n_numbers > 0).mean(), 100 * feat.has_percent.mean(), 100 * feat.has_year.mean()))
print("\nNRC emotion (corpus mean frequency):")
print(feat[[c for c in feat if c.startswith("nrc_")]].mean().round(3).sort_values(ascending=False).to_string())
print("\nMoral Foundations (mean hits/100w):")
print(feat[[c for c in feat if c.startswith("mfd_") and c != "mfd_total"]].mean().round(3).sort_values(ascending=False).to_string())
print("\nLanguage distribution:")
print(feat.lang.value_counts().head(8).to_string())
print("\nHashtag classes (post-tag rows):")
print(pd.DataFrame(tags_long).tag_class.value_counts().to_string() if tags_long else "  (none)")
print("\nTop 12 hashtags:"); print(tag_top.head(12).to_string(index=False))
print("\nTop 10 emoji:"); print(emo_top.head(10).to_string(index=False))
