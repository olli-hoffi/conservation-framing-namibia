#!/usr/bin/env python3
"""
Phase C - Theme 3 (data-generation). Named entities from captions on the FULL coded
corpus (N=917): species (curated conservation gazetteer + IUCN Red List status),
places (Namibia gazetteer + spaCy GPE/LOC supplement), and organisations
(@mention network edges + spaCy ORG for named partners / funders).

Species detection is gazetteer-based on purpose: spaCy has no SPECIES type, and the
gazetteer carries the canonical name needed for the IUCN join. IUCN statuses are
species-level global Red List categories (public); where a taxon is split into
subspecies the species-level assessment (or the locally relevant listing) is used,
so treat the attention-vs-threat table as exploratory. spaCy en_core_web_sm adds
GPE/LOC/ORG surface mentions.

Reads : data/raw/Study1/Study1_Posts_RAW_20260520.csv, analysis_base.csv
Writes: output/entities_species_long.csv, entities_species_summary.csv,
        entities_places_long.csv, entities_place_summary.csv,
        entities_mentions_edges.csv, entities_org_edges.csv, entities_ner_orgs_long.csv
Run   : "../4. NLP Pipeline/.venv/bin/python" c5_entity_features.py
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
# WHY entities: which SPECIES and PLACES a conservation account talks about, and which
# other orgs it @mentions, describe what the sector foregrounds. The headline
# exploratory question is "attention vs. threat": do orgs post about the most
# endangered species (IUCN CR/EN), or about charismatic-but-safer ones? The @mention
# and shared-org tables sketch the collaboration network between accounts.
import os, re, glob
from collections import Counter, defaultdict
import pandas as pd
import spacy                   # spaCy: named-entity recognition for places/orgs the gazetteers miss

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

# ---- species gazetteer: canonical -> (surface patterns, IUCN status) ----------
# Categories checked against the IUCN Red List, version 2026-1, on 2026-08-20.
# IUCN Red List categories (public assessments). Rank: LC<NT<VU<EN<CR.
# Each entry maps a canonical species name to (a) the surface forms to search for in
# captions and (b) its IUCN threat category. LC=Least Concern ... CR=Critically
# Endangered. The canonical name is what the attention-vs-threat table groups on.
SPECIES = {
    "Black rhino":        (["black rhino", "black rhinoceros", "hook-lipped rhino"], "CR"),
    "White rhino":        (["white rhino", "white rhinoceros", "square-lipped rhino"], "NT"),
    "Rhino (unspecified)":(["rhino", "rhinoceros", "rhinos"], "CR"),
    "African elephant":   (["elephant", "elephants", "desert elephant", "loxodonta"], "EN"),
    "Cheetah":            (["cheetah", "cheetahs", "acinonyx"], "VU"),
    "Lion":               (["lion", "lions", "desert lion", "desert-adapted lion", "panthera leo"], "VU"),
    "Leopard":            (["leopard", "leopards"], "VU"),
    "African wild dog":   (["wild dog", "wild dogs", "painted dog", "painted wolf", "lycaon"], "EN"),
    "Pangolin":           (["pangolin", "pangolins", "scaly anteater"], "VU"),
    "Giraffe":            (["giraffe", "giraffes", "angolan giraffe", "giraffa"], "VU"),
    "Hippopotamus":       (["hippo", "hippos", "hippopotamus"], "VU"),
    "Mountain zebra":     (["mountain zebra", "hartmann's zebra", "hartmann zebra"], "VU"),
    "Plains zebra":       (["plains zebra", "burchell's zebra", "zebra", "zebras"], "NT"),
    "Oryx / gemsbok":     (["oryx", "gemsbok"], "LC"),
    "Springbok":          (["springbok", "springbuck"], "LC"),
    "Kudu":               (["kudu", "kudus"], "LC"),
    "Eland":              (["eland"], "LC"),
    "Sable antelope":     (["sable antelope", "sable"], "LC"),
    "Roan antelope":      (["roan antelope", "roan"], "LC"),
    "Hartebeest":         (["hartebeest", "red hartebeest"], "LC"),
    "Wildebeest":         (["wildebeest", "gnu", "blue wildebeest", "black wildebeest"], "LC"),
    "Warthog":            (["warthog", "warthogs"], "LC"),
    "Buffalo":            (["buffalo", "cape buffalo", "african buffalo"], "NT"),
    "Brown hyena":        (["brown hyena", "brown hyaena", "strandwolf"], "NT"),
    "Spotted hyena":      (["spotted hyena", "spotted hyaena", "hyena", "hyaena"], "LC"),
    "Caracal":            (["caracal"], "LC"),
    "Serval":             (["serval"], "LC"),
    "African wildcat":    (["african wildcat", "wildcat"], "LC"),
    "Black-backed jackal":(["jackal", "black-backed jackal"], "LC"),
    "Bat-eared fox":      (["bat-eared fox"], "LC"),
    "Cape fox":           (["cape fox"], "LC"),
    "Aardvark":           (["aardvark", "antbear"], "LC"),
    "Aardwolf":           (["aardwolf"], "LC"),
    "Honey badger":       (["honey badger", "ratel"], "LC"),
    "Meerkat":            (["meerkat", "suricate"], "LC"),
    "Baboon":             (["baboon", "chacma baboon"], "LC"),
    "Vervet monkey":      (["vervet", "vervet monkey"], "LC"),
    "Cape fur seal":      (["fur seal", "cape fur seal", "seal", "seals"], "LC"),
    "African penguin":    (["african penguin", "penguin", "penguins"], "CR"),
    "Ostrich":            (["ostrich", "ostriches"], "LC"),
    "Kori bustard":       (["kori bustard", "bustard"], "NT"),
    "Lesser flamingo":    (["lesser flamingo", "flamingo", "flamingos"], "NT"),
    "Southern ground hornbill":(["ground hornbill"], "VU"),
    "White-backed vulture":(["white-backed vulture"], "CR"),
    "Lappet-faced vulture":(["lappet-faced vulture"], "EN"),
    "Cape vulture":       (["cape vulture"], "VU"),
    "Vulture (unspecified)":(["vulture", "vultures"], "EN"),
    "Damara tern":        (["damara tern"], "VU"),
    "African fish eagle": (["fish eagle"], "LC"),
    "Martial eagle":      (["martial eagle"], "EN"),
    "Pel's fishing owl":  (["pel's fishing owl"], "LC"),
    "Heaviside's dolphin":(["heaviside's dolphin", "benguela dolphin", "dolphin", "dolphins"], "NT"),
    "Humpback whale":     (["humpback whale", "whale", "whales"], "LC"),
    "Leatherback turtle": (["leatherback"], "VU"),
    "Green turtle":       (["green turtle"], "LC"),   # downlisted EN -> LC, October 2025
    "Loggerhead turtle":  (["loggerhead", "sea turtle", "turtle", "turtles"], "VU"),
    "Nile crocodile":     (["crocodile", "nile crocodile"], "LC"),
    "African rock python":(["python", "rock python"], "NT"),
    "Pygmy falcon":       (["pygmy falcon"], "LC"),
}
# generic buckets are dropped for a post when a more specific species in the same
# animal group also matched (a "black rhino" post should not also count as generic
# rhino, nor "mountain zebra" as plains zebra) - avoids double counting across
# IUCN categories in the attention-vs-threat table.
# Read as {generic_name: [specific_names that suppress it]}.
GENERIC_SUPPRESS = {
    "Rhino (unspecified)": ["Black rhino", "White rhino"],
    "Plains zebra": ["Mountain zebra"],
    "Spotted hyena": ["Brown hyena"],
    "Loggerhead turtle": ["Leatherback turtle", "Green turtle"],
    "Vulture (unspecified)": ["White-backed vulture", "Lappet-faced vulture", "Cape vulture"],
}
IUCN_RANK = {"LC": 0, "NT": 1, "VU": 2, "EN": 3, "CR": 4}   # ordinal threat level (higher = more endangered)
# Pre-compile one regex per species: alternation of its surface forms, escaped and
# sorted longest-first so "black rhino" is tried before "rhino". \b word boundaries
# and re.I make matching whole-word and case-insensitive.
species_re = {c: re.compile(r"\b(?:" + "|".join(re.escape(p) for p in sorted(pats, key=len, reverse=True)) + r")\b", re.I)
              for c, (pats, _) in SPECIES.items()}
species_status = {c: st for c, (_, st) in SPECIES.items()}   # canonical name -> IUCN status lookup

# ---- place gazetteer ----------------------------------------------------------
# Curated Namibian geography: regions, parks/protected areas, towns/features,
# neighbouring countries. Lets us count which places the sector foregrounds.
PLACES = {
    # regions
    "Kunene": "region", "Erongo": "region", "Zambezi": "region", "Kavango": "region",
    "Otjozondjupa": "region", "Omaheke": "region", "Hardap": "region", "Karas": "region",
    "Khomas": "region", "Oshana": "region", "Ohangwena": "region", "Omusati": "region",
    "Oshikoto": "region",
    # parks / protected areas
    "Etosha": "park", "Namib-Naukluft": "park", "Skeleton Coast": "park", "Bwabwata": "park",
    "Mudumu": "park", "Nkasa Rupara": "park", "Waterberg": "park", "Dorob": "park",
    "Khaudum": "park", "Mangetti": "park", "Ai-Ais": "park", "Sperrgebiet": "park",
    "Tsau Khaeb": "park", "Cape Cross": "park", "NamibRand": "park",
    # towns / places / features
    "Windhoek": "town", "Swakopmund": "town", "Walvis Bay": "town", "Luderitz": "town",
    "Otjiwarongo": "town", "Kamanjab": "town", "Opuwo": "town", "Gobabeb": "place",
    "Sossusvlei": "place", "Sesriem": "place", "Twyfelfontein": "place", "Palmwag": "place",
    "Okonjima": "place", "Damaraland": "region", "Kaokoland": "region", "Kaokoveld": "region",
    "Caprivi": "region", "Namib Desert": "feature", "Namib": "feature", "Kalahari": "feature",
    "Kunene River": "feature", "Kavango River": "feature", "Zambezi River": "feature",
    "Kwando": "feature", "Orange River": "feature",
    # countries (context)
    "Namibia": "country", "Angola": "country", "Botswana": "country", "South Africa": "country",
}
places_re = {p: re.compile(r"\b" + re.escape(p) + r"\b", re.I) for p in PLACES}   # one whole-word, case-insensitive regex per place

MENTION = re.compile(r"@([A-Za-z0-9._]+)")   # capture the handle AFTER the @ (group 1) for the mention network

# ---- load captions + org handles ----------------------------------------------
# Raw corpus -> {Post_ID: caption}; analysis_base -> the coded corpus with its
# org handles. own_handles/id2org let us tell "mentions another CORPUS org" from
# "mentions an outside account", and label each post with its source org.
capdf = pd.read_csv(os.path.join(str(_RAW), "Study1", "Study1_Posts_RAW_20260520.csv"),
                    sep=";", encoding="utf-8-sig", dtype=str)
base = pd.read_csv(str(_BASE), sep=";", encoding="utf-8-sig")
coded = base[base["coded"] == 1][["Post_ID", "Actor_Name", "Handle"]].copy()
coded["Handle"] = coded["Handle"].astype(str).str.lower()
cap = dict(zip(capdf["Post_ID"], capdf["Caption_Text"].fillna("")))
own_handles = set(coded["Handle"])                       # the set of handles that belong to corpus orgs
id2org = dict(zip(coded["Post_ID"], coded["Handle"]))    # Post_ID -> its own org handle (the "source" of the mention)

print("Loading spaCy en_core_web_sm ...")
# Small English model, with the pipes we do not need disabled for speed (we only use
# the NER component here, not lemmatiser/tagger).
nlp = spacy.load("en_core_web_sm", disable=["lemmatizer", "tagger", "attribute_ruler"])

# ---- extract ------------------------------------------------------------------
# Long-format accumulators: one row per (post, entity). Long format is what R prefers
# for counting/joining downstream.
sp_long, pl_long, ment_edges, ner_org_long = [], [], [], []
texts = [(pid, cap.get(pid, "")) for pid in coded["Post_ID"]]   # (id, caption) pairs in corpus order

for pid, text in texts:
    src = id2org.get(pid, "")                            # the org that authored this post
    # species (post-level presence), then suppress generic when a specific matched
    # Walruses (:=) both run the regex and keep the match list; hits = {species: count}.
    hits = {canon: len(m) for canon, rx in species_re.items() if (m := rx.findall(text))}
    for generic, specifics in GENERIC_SUPPRESS.items():
        if generic in hits and any(s in hits for s in specifics):
            del hits[generic]                            # drop the generic bucket when a specific sibling is present
    for canon, n in hits.items():
        sp_long.append({"Post_ID": pid, "src_org": src, "species": canon,
                        "iucn_status": species_status[canon], "n_mentions": n})
    # places (gazetteer) - record every gazetteer place whose regex matches this caption
    for place, rx in places_re.items():
        if rx.search(text):
            pl_long.append({"Post_ID": pid, "src_org": src, "place": place,
                            "place_type": PLACES[place], "source": "gazetteer"})
    # @mentions -> org network edges (directed: this post's org -> the mentioned handle)
    for h in MENTION.findall(text):
        h = h.lower().rstrip(".")                        # normalise; strip a trailing dot from "@handle."
        if h and h != src:                               # ignore self-mentions
            ment_edges.append({"Post_ID": pid, "src_org": src, "mentioned": h,
                               "is_own_org": int(h in own_handles)})   # 1 if the mentioned handle is another corpus org

# spaCy pass (GPE/LOC supplement + ORG partners)
# Run the NER model over all captions in a batched pipe (fast). GPE/LOC/FAC entities
# NOT already in the gazetteer become extra place rows; ORG entities become named
# partner/funder rows. len(t) > 2 filters out noise like initials.
gaz_places_lower = {p.lower() for p in PLACES}           # lower-cased gazetteer names, to avoid double-recording a place
for pid, doc in zip(coded["Post_ID"], nlp.pipe([t for _, t in texts], batch_size=64)):
    src = id2org.get(pid, "")
    for ent in doc.ents:
        t = ent.text.strip()
        if ent.label_ in ("GPE", "LOC", "FAC") and t.lower() not in gaz_places_lower and len(t) > 2:
            pl_long.append({"Post_ID": pid, "src_org": src, "place": t,
                            "place_type": ent.label_, "source": "spacy"})   # tag source so gazetteer vs spaCy stays distinguishable
        elif ent.label_ == "ORG" and len(t) > 2:
            ner_org_long.append({"Post_ID": pid, "src_org": src, "org_text": t})

# ---- write long tables --------------------------------------------------------
sp = pd.DataFrame(sp_long); pl = pd.DataFrame(pl_long)
ment = pd.DataFrame(ment_edges); nerorg = pd.DataFrame(ner_org_long)
sp.to_csv(os.path.join(OUT, "entities_species_long.csv"), index=False)
pl.to_csv(os.path.join(OUT, "entities_places_long.csv"), index=False)
ment.to_csv(os.path.join(OUT, "entities_mentions_edges.csv"), index=False)
nerorg.to_csv(os.path.join(OUT, "entities_ner_orgs_long.csv"), index=False)

# ---- species attention-vs-threat summary --------------------------------------
# Per species: in how many posts / by how many orgs it appears, and total mentions.
# n_posts uses nunique so multiple mentions in one post count once. iucn_rank lets R
# correlate "attention" (posts) against "threat" (rank) - the headline exploratory plot.
sp_sum = (sp.groupby(["species", "iucn_status"])
          .agg(n_posts=("Post_ID", "nunique"), n_orgs=("src_org", "nunique"),
               total_mentions=("n_mentions", "sum")).reset_index())
sp_sum["iucn_rank"] = sp_sum["iucn_status"].map(IUCN_RANK)
sp_sum = sp_sum.sort_values("n_posts", ascending=False)
sp_sum.to_csv(os.path.join(OUT, "entities_species_summary.csv"), index=False)

# ---- place summary ------------------------------------------------------------
# Per place (keeping its type + whether gazetteer or spaCy found it): post + org reach.
pl_sum = (pl.groupby(["place", "place_type", "source"])
          .agg(n_posts=("Post_ID", "nunique"), n_orgs=("src_org", "nunique")).reset_index()
          .sort_values("n_posts", ascending=False))
pl_sum.to_csv(os.path.join(OUT, "entities_place_summary.csv"), index=False)

# ---- aggregated org->org mention edges ----------------------------------------
# Collapse individual mentions into weighted directed edges (how many times src_org
# mentioned each handle). is_own_org kept so the corpus-internal network can be split
# out. Guarded for the empty case.
if len(ment):
    org_edges = (ment.groupby(["src_org", "mentioned", "is_own_org"])
                 .size().reset_index(name="weight").sort_values("weight", ascending=False))
    org_edges.to_csv(os.path.join(OUT, "entities_org_edges.csv"), index=False)
else:
    org_edges = pd.DataFrame()

# ---- audit --------------------------------------------------------------------
# Console summary: row counts per entity type, the top species (attention-vs-threat),
# attention split by IUCN category, top places, and the strongest within-corpus mentions.
print(f"\nc5_entity_features.py:")
print(f"  species: {len(sp)} post-species rows, {sp_sum.shape[0]} distinct species")
print(f"  places : {len(pl)} rows ({(pl.source=='gazetteer').sum()} gazetteer, {(pl.source=='spacy').sum()} spaCy)")
print(f"  mentions: {len(ment)} @mention edges ({int(ment.is_own_org.sum()) if len(ment) else 0} to other corpus orgs)")
print(f"  spaCy ORG mentions: {len(nerorg)} rows")
print("\nSpecies attention vs IUCN threat (top 15 by posts):")
print(sp_sum.head(15).to_string(index=False))
print("\nAttention by IUCN category (share of species-post rows):")
# Distinct posts mentioning any species in each threat band, ordered least->most endangered.
byrank = sp.groupby("iucn_status").Post_ID.nunique().reindex(["LC","NT","VU","EN","CR"]).fillna(0).astype(int)
print(byrank.to_string())
print("\nTop places:")
print(pl_sum.head(12).to_string(index=False))
print("\nTop org->org @mentions (within corpus):")
if len(org_edges):
    print(org_edges[org_edges.is_own_org==1].head(10).to_string(index=False))   # only edges to other corpus orgs