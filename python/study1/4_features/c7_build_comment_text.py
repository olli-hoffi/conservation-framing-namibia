"""
c7a - Build the pseudonymised comment-text file for the reproduction package.

The Study 1 reception paragraph reports the shape of the retrieved comments
(emoji only, question marks, length, repeat commenters). Those figures had no
script in the package, because the only file carrying comment TEXT is the raw
Apify scrape, and that file holds the public usernames of private individuals.
The thesis states that no private commenter's username appears in the shared
data, so the raw scrape must not be redistributed.

This script bridges that gap. It joins the raw scrape to the already
pseudonymised analysis corpus on comment_id, carries over the stable pseudonym
(C0001 ...) assigned there, scrubs @mentions of private accounts inside the
comment text, and writes a text file that carries no private identifier.
Organisational handles are deliberately left in clear, as they are throughout
the thesis.

Run once, from anywhere. The raw scrape stays outside the package.

Input : 03_Methods-Data/.../Comment Data Apify/comments_merged.json   (not shared)
        data/study1/features/comments_labeled.csv                     (pseudonymised)
Output: data/study1/comments_text.csv
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
# Status: RECORD ONLY. It reads the raw Apify comment export, a JSON the package
# does not ship. Its output, comments_text.csv, is included and is what the R side
# and the other feature scripts read.
#
import csv
import json
import os
import re

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.abspath(os.path.join(HERE, "..", ".."))
VAULT = os.path.abspath(os.path.join(ROOT, ".."))

RAW = os.path.join(
    VAULT, "03_Methods-Data", "1. Account & Content Analysis",
    "2. Data Collection (Apify)", "Comment Data Apify", "comments_merged.json")
LABELLED = os.path.join(ROOT, "data", "study1", "features", "comments_labeled.csv")
OUT = os.path.join(ROOT, "data", "study1", "comments_text.csv")

MENTION = re.compile(r"@([A-Za-z0-9._]+)")


def main():
    raw = json.load(open(RAW, encoding="utf-8"))
    labelled = {r["comment_id"]: r
                for r in csv.DictReader(open(LABELLED, encoding="utf-8"))}

    # Public handle -> stable pseudonym, taken from the corpus that already carries them.
    handle_to_pseudonym = {}
    for c in raw:
        cid = str(c.get("id") or "")
        handle = c.get("ownerUsername")
        if cid in labelled and handle:
            handle_to_pseudonym.setdefault(handle.lower(), labelled[cid]["commenter"])

    def scrub(text):
        if not text:
            return ""
        def swap(m):
            handle = m.group(1).lower()
            # Only private commenters are replaced; organisational handles stay.
            return "@" + handle_to_pseudonym[handle] if handle in handle_to_pseudonym else m.group(0)
        return MENTION.sub(swap, text)

    rows = []
    for c in raw:
        cid = str(c.get("id") or "")
        if cid not in labelled:
            continue          # one null record in the scrape is not part of the corpus
        rows.append({"comment_id": cid,
                     "Post_ID": labelled[cid]["Post_ID"],
                     "commenter": labelled[cid]["commenter"],
                     "text": scrub(c.get("text") or "")})

    with open(OUT, "w", newline="", encoding="utf-8") as fh:
        writer = csv.DictWriter(fh, fieldnames=["comment_id", "Post_ID", "commenter", "text"])
        writer.writeheader()
        writer.writerows(rows)

    # Leak check: no private handle may survive anywhere in the written file.
    blob = open(OUT, encoding="utf-8").read().lower()
    leaks = [h for h in handle_to_pseudonym
             if re.search(r"(^|[^a-z0-9._])" + re.escape(h) + r"([^a-z0-9._]|$)", blob)]

    print(f"raw scrape       : {len(raw)} records")
    print(f"analysis corpus  : {len(labelled)} comments")
    print(f"written          : {len(rows)} rows -> {OUT}")
    print(f"private handles  : {len(handle_to_pseudonym)} mapped to pseudonyms")
    print(f"leak check       : {len(leaks)} private handles remaining"
          + (f"  {leaks[:5]}" if leaks else ""))


if __name__ == "__main__":
    main()
