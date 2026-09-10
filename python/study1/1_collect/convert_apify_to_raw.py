#!/usr/bin/env python3
"""
convert_apify_to_raw.py
-----------------------
Converts Apify instagram-scraper JSON output to the Study 1 RAW format
expected by Study1_Content_Analysis_CODER_A.csv.

Usage:
    python3 convert_apify_to_raw.py

Input:  Apify JSON file in ./Data from Apify/  (auto-detected, most recent)
Output: Study1_Posts_RAW_<date>.csv  (semicolon-delimited, UTF-8-BOM)
        Study1_CODER_A_template_<date>.csv  (pre-filled metadata for coding)
        data/study1/retrieval_cap_audit.csv
        data/study1/retrieval_cap_audit_summary.csv

Two output files:
  1. RAW csv  — full metadata including Post_URL, Actor_Name, Caption_Text
  2. CODER_A template — matches CODER_A column order; coding columns left blank
"""
#
# PATHS IN THIS PACKAGE
# This script was written inside the thesis working tree, where its inputs sat in
# sibling folders named "2. Data Collection (Apify)" and the like. In the
# reproduction package the same files live under data/, and python/_paths.py
# holds the canonical locations.
#
# Status: RECORD ONLY. It cannot run here, because it needs the raw Apify JSON export, which is not shipped.
# The outputs it produced are shipped and are the canonical record, so nothing in
# the reported results depends on rerunning it.
#

# Standard-library only (no third-party install needed):
#   csv  - write the two semicolon-delimited output sheets
#   glob - find the most recent Apify JSON export in the input folder
#   json - parse the scraped Instagram data
#   os   - build file paths that work regardless of where the script is launched
#   datetime/timezone - parse the ISO post timestamps and filter the sampling window
import csv
import glob
import json
import os
from collections import Counter, defaultdict
from datetime import datetime, timezone
from urllib.parse import urlparse

# ── Configuration ──────────────────────────────────────────────────────────────
# Script lives in `1. Account & Content Analysis/2. Data Collection (Apify)/`.
# RAW CSV (this phase's output) → same folder.
# CODER_A template (coding-phase input) → `3. Codebook & Coding/` (sibling folder).
SCRIPT_DIR  = os.path.dirname(os.path.abspath(__file__))   # absolute path of THIS file's folder
PARENT_DIR  = os.path.dirname(SCRIPT_DIR)  # → "1. Account & Content Analysis/"
APIFY_DIR   = os.path.join(SCRIPT_DIR, "Data from Apify")   # where the scraped JSON lives
CODING_DIR  = os.path.join(PARENT_DIR, "3. Codebook & Coding")   # where the coder template is written
RUN_DATE    = datetime.now().strftime("%Y%m%d")   # e.g. 20260520; stamped into both output filenames
PACKAGE_ROOT = os.path.abspath(os.path.join(SCRIPT_DIR, "..", "..", ".."))
RETRIEVAL_DETAIL_PATH = os.path.join(PACKAGE_ROOT, "data", "study1", "retrieval_cap_audit.csv")
RETRIEVAL_SUMMARY_PATH = os.path.join(PACKAGE_ROOT, "data", "study1", "retrieval_cap_audit_summary.csv")

# Study 1 sampling window: 1 Nov 2025 (inclusive) to 1 May 2026 (exclusive).
# tzinfo=UTC so the comparison against each post's UTC timestamp is apples-to-apples.
# This is the fixed 6-month observation window that defines the corpus.
SAMPLING_START = datetime(2025, 11, 1, tzinfo=timezone.utc)
SAMPLING_END   = datetime(2026,  5, 1, tzinfo=timezone.utc)
LATE_SERIES_START = datetime(2026, 2, 1, tzinfo=timezone.utc)

# ── Target organizations ───────────────────────────────────────────────────────
# The sampling frame: only posts from these 21 accounts enter the corpus (any
# other handle in the scrape is dropped as "wrong org"). This dict is the
# canonical handle → (Actor_ID, Actor_Name, Actor_Type) lookup.
# Actor_Type is the numeric code from the Study 1 codebook actor typology
# (governmental / NGO / community-based / private sector / hybrid / research etc.);
# the codebook holds the full legend. Observed here: 1 dominates (NGO/trust/
# foundation), 2 = research centre (Gobabeb), 3 = community/youth bodies (NACSO,
# youth chambers), 4 = government ministry, 5 = private reserve (Ongava), 6 = EIF.
# "# pending verification" flags handles not yet confirmed at scrape time.
# handle → (Actor_ID, Actor_Name, Actor_Type)
TARGET_ORGS = {
    "africat_foundation":          ("A0001", "AfriCat Foundation",                          1),
    "ccfcheetah":                  ("A0011", "Cheetah Conservation Fund",                   1),
    "desertlionconservation":      ("A0015", "Desert Lion Conservation Trust",              1),
    "eduventuresnamibia":          ("A0019", "EduVentures Trust",                           1),  # pending verification
    "ehranamibia":                 ("A0020", "Elephant Human Relations Aid",                1),
    "eif_namibia":                 ("A0021", "Environmental Investment Fund",               6),
    "giraffe_conservation":        ("A0022", "Giraffe Conservation Foundation",             1),
    "gobabeb":                     ("A0023", "Gobabeb Research & Training Centre",          2),
    "harnaswildlifefoundation":    ("A0025", "Harnas Wildlife Foundation",                  1),
    "ministry_of_environment_nam": ("A0033", "Ministry of Environment Forestry & Tourism",  4),
    "nacso_namibia1":              ("A0035", "NACSO",                                       3),
    "namibia_nature_foundation":   ("A0037", "Namibia Nature Foundation",                   1),
    "namyouthchamber":             ("A0040", "Namibia Youth Chamber of Environment",        3),
    "naankuse_foundation":         ("A0042", "N/a'an ku se Foundation",                    1),
    "oceanconservationnamibia":    ("A0044", "Ocean Conservation Namibia",                  1),
    "ongavagamereserve":           ("A0045", "Ongava Game Reserve",                         5),
    "savetherhinonamibia":         ("A0048", "Save the Rhino Trust Namibia",               1),
    "thinknamibia":                ("A0050", "TH!NK Namibia",                               1),  # pending verification
    "tosconamibia":                ("A0052", "Tosco Trust",                                 1),
    "wwfnamibia":                  ("A0053", "WWF Namibia",                                 1),
    "youth4can_namibia":           ("A0054", "Youth4CAN",                                   3),
}

# Translate Apify's post-type vocabulary into the thesis codebook's Post_Format
# labels. Apify calls a multi-image post "Sidecar"; the thesis calls it "Carousel".
# Video needs a manual Reel-vs-Video check later (Apify does not distinguish Reels),
# hence Reel_Check_Needed is set to YES downstream for every Video.
# Apify type → thesis format
FORMAT_MAP = {
    "Image":   "Image",
    "Sidecar": "Carousel",
    "Video":   "Video",   # Reel_Check_Needed=YES for all Video posts
}

# Column order for the RAW export: post identity + metadata + engagement + caption.
# No coding columns here - the RAW file is the raw evidence, not the coding sheet.
RAW_COLUMNS = [
    "Post_ID", "Post_URL", "Actor_ID", "Actor_Name", "Actor_Type",
    "Instagram_Handle", "Posting_Date", "Post_Format", "Reel_Check_Needed",
    "Likes", "Comments", "Caption_Text",
]

# The coder's working sheet: the same metadata as RAW, then all 19 codebook
# variables appended as empty columns for the human coder to fill in by hand.
# The blank block below (Language ... Data_Visual) is exactly the codebook's
# 6 domains flattened into one row per post.
# CODER_A template: metadata pre-filled; coding columns left blank
CODER_A_COLUMNS = [
    "Post_ID", "Post_URL", "Actor_ID", "Actor_Name", "Actor_Type",
    "Instagram_Handle", "Posting_Date", "Post_Format", "Reel_Check_Needed",
    "Likes", "Comments", "Caption_Text",
    # Coding variables — blank for manual entry
    "Language", "Primary_Topic",
    "Empathy", "Threat", "Efficacy", "Collective_ID", "Normative",
    "Moral", "Economic", "Scientific",
    "Primary_CTA",
    "Youth_Addressed", "Youth_Style", "Interactivity",
    "Protagonist", "Emotional_Valence", "Story_Structure",
    "Onscreen_Text", "Data_Visual",
]


def find_apify_json():
    """Return path to most-recent Apify JSON in the Data from Apify folder."""
    pattern = os.path.join(APIFY_DIR, "*.json")
    files = sorted(glob.glob(pattern))       # all JSONs, sorted by name
    if not files:
        raise FileNotFoundError(f"No JSON files found in {APIFY_DIR}")
    latest = files[-1]                       # last after sort = newest (filenames are date-stamped)
    print(f"Using Apify file: {os.path.basename(latest)}")
    return latest


def parse_timestamp(ts_str):
    """Parse ISO timestamp string → aware datetime."""
    if not ts_str:
        return None
    # Instagram/Apify write the trailing "Z" (Zulu = UTC); fromisoformat() on this
    # Python does not accept "Z", so swap it for the explicit "+00:00" offset.
    # Result is a timezone-aware datetime, comparable to SAMPLING_START/END.
    return datetime.fromisoformat(ts_str.replace("Z", "+00:00"))


def requested_handle(input_url):
    """Extract the requested Instagram handle from an Apify input URL."""
    return urlparse(input_url).path.strip("/").split("/")[0].lower()


def write_retrieval_audit(data):
    """Save aggregate request-depth and retained-series diagnostics."""
    grouped = defaultdict(list)
    for record in data:
        grouped[requested_handle(record["inputUrl"])].append(record)

    requested_handles = set(grouped)
    detail = []
    for handle, request_rows in sorted(grouped.items()):
        own_in_request = [
            row for row in request_rows
            if str(row.get("ownerUsername", "")).lower() == handle
        ]
        own = [
            row for row in data
            if str(row.get("ownerUsername", "")).lower() == handle
            and str(row.get("ownerUsername", "")).lower() in requested_handles
        ]
        own_dated = [
            (row, parse_timestamp(row["timestamp"]))
            for row in own if row.get("timestamp")
        ]
        in_window = [
            (row, timestamp) for row, timestamp in own_dated
            if SAMPLING_START <= timestamp < SAMPLING_END
        ]
        dates = [timestamp for _, timestamp in in_window]
        detail.append({
            "requested_handle": handle,
            "raw_records": len(request_rows),
            "own_profile_records_in_request": len(own_in_request),
            "other_owner_records_in_request": len(request_rows) - len(own_in_request),
            "all_records_owned_by_profile": len(own),
            "own_records_before_window": sum(timestamp < SAMPLING_START for _, timestamp in own_dated),
            "own_records_after_window": sum(timestamp >= SAMPLING_END for _, timestamp in own_dated),
            "retained_window_records": len(in_window),
            "first_retained_post": min(dates).date().isoformat() if dates else "",
            "last_retained_post": max(dates).date().isoformat() if dates else "",
            "series_begins_february_or_later": bool(
                dates and min(dates) >= LATE_SERIES_START
            ),
        })

    maximum = max(row["raw_records"] for row in detail)
    summary = [{
        "n_raw_records": len(data),
        "n_requested_profiles": len(detail),
        "n_profiles_with_retained_posts": sum(
            row["retained_window_records"] > 0 for row in detail
        ),
        "maximum_raw_records_per_profile": maximum,
        "n_profiles_at_maximum": sum(
            row["raw_records"] == maximum for row in detail
        ),
        "n_retained_window_posts": sum(
            row["retained_window_records"] for row in detail
        ),
        "n_series_beginning_february_or_later": sum(
            row["series_begins_february_or_later"] for row in detail
        ),
        "window_start": SAMPLING_START.date().isoformat(),
        "window_end_inclusive": "2026-04-30",
    }]

    for path, audit_rows in (
        (RETRIEVAL_DETAIL_PATH, detail),
        (RETRIEVAL_SUMMARY_PATH, summary),
    ):
        os.makedirs(os.path.dirname(path), exist_ok=True)
        with open(path, "w", encoding="utf-8", newline="") as output_file:
            writer = csv.DictWriter(output_file, fieldnames=list(audit_rows[0]))
            writer.writeheader()
            writer.writerows(audit_rows)

    print(f"Raw records per requested profile: maximum {maximum}")
    print(
        "Retained series beginning in February or March 2026: "
        f"{summary[0]['n_series_beginning_february_or_later']}"
    )


def convert():
    json_path = find_apify_json()
    with open(json_path, encoding="utf-8") as f:
        data = json.load(f)          # the whole scrape as a list of post records (dicts)

    write_retrieval_audit(data)

    rows = []                        # accumulates one output dict per KEPT post
    skipped_outside_window = 0       # counters, purely for the run summary at the end
    skipped_wrong_org = 0
    warnings = []

    # Walk every scraped record and apply the three inclusion gates in order:
    # (1) is it one of our target orgs, (2) does it have a timestamp,
    # (3) is that timestamp inside the sampling window.
    for record in data:
        handle = record.get("ownerUsername") or ""   # the posting account; "" if missing
        if handle not in TARGET_ORGS:
            skipped_wrong_org += 1   # scrape may pull in reshares/tags from other accounts
            continue                 # drop: not part of the sampling frame

        ts = parse_timestamp(record.get("timestamp"))
        if ts is None:
            # No usable date = cannot place it in/out of the window; record it and skip.
            warnings.append(f"  ⚠ No timestamp for post {record.get('shortCode')} (@{handle}) — skipped")
            continue

        # Half-open window [START, END): 1 Nov 2025 is kept, 1 May 2026 is excluded.
        if not (SAMPLING_START <= ts < SAMPLING_END):
            skipped_outside_window += 1
            continue

        # Passed all gates -> build the output row from the three-tuple + fields.
        actor_id, actor_name, actor_type = TARGET_ORGS[handle]   # unpack the org metadata
        raw_fmt = record.get("type", "")
        fmt = FORMAT_MAP.get(raw_fmt, raw_fmt)  # keep unknown types as-is
        reel_check = "YES" if fmt == "Video" else "NO"   # Videos need a manual Reel check later

        # Caption cleanup for CSV safety: newlines -> spaces (keep one row per post) and
        # semicolons -> commas (";" is the column delimiter, so it must not appear in text).
        caption = (record.get("caption") or "").replace("\n", " ").replace(";", ",").strip()
        shortcode = record.get("shortCode", "")   # IG short code = the stable Post_ID

        rows.append({
            "Post_ID":           shortcode,
            "Post_URL":          record.get("url") or f"https://www.instagram.com/p/{shortcode}/",  # rebuild URL if absent
            "Actor_ID":          actor_id,
            "Actor_Name":        actor_name,
            "Actor_Type":        actor_type,
            "Instagram_Handle":  handle,
            "Posting_Date":      ts.strftime("%Y-%m-%d"),   # date only (drop time) for the coding sheet
            "Post_Format":       fmt,
            "Reel_Check_Needed": reel_check,
            "Likes":             record.get("likesCount") or 0,     # "or 0" -> missing/None becomes 0
            "Comments":          record.get("commentsCount") or 0,
            "Caption_Text":      caption,
        })

    # ── Write RAW file ─────────────────────────────────────────────────────────
    # utf-8-sig writes a BOM so Excel opens the accented org names correctly;
    # delimiter=";" matches the whole pipeline; extrasaction="ignore" means any
    # dict key not in RAW_COLUMNS is silently dropped (here they all match).
    raw_path = os.path.join(SCRIPT_DIR, f"Study1_Posts_RAW_{RUN_DATE}.csv")
    with open(raw_path, "w", newline="", encoding="utf-8-sig") as f:
        writer = csv.DictWriter(f, fieldnames=RAW_COLUMNS, delimiter=";", extrasaction="ignore")
        writer.writeheader()
        writer.writerows(rows)

    # ── Write CODER_A template ─────────────────────────────────────────────────
    # Goes to the sibling Phase-3 folder so coders work directly in the coding folder.
    os.makedirs(CODING_DIR, exist_ok=True)   # create "3. Codebook & Coding/" if it does not exist
    coder_path = os.path.join(CODING_DIR, f"Study1_CODER_A_template_{RUN_DATE}.csv")
    with open(coder_path, "w", newline="", encoding="utf-8-sig") as f:
        writer = csv.DictWriter(f, fieldnames=CODER_A_COLUMNS, delimiter=";",
                                extrasaction="ignore")
        writer.writeheader()
        # Same metadata rows, but pad every missing coding column with "" so the
        # coder opens a sheet whose 19 codebook columns are present and blank.
        # Add blank coding columns
        for row in rows:
            row_out = dict(row)                 # copy so we do not mutate the RAW row
            for col in CODER_A_COLUMNS:
                if col not in row_out:
                    row_out[col] = ""           # blank cell = "not yet coded"
            writer.writerow(row_out)

    # ── Summary ────────────────────────────────────────────────────────────────
    # Console tally so the corpus can be sanity-checked at a glance: totals kept/
    # dropped, post-format mix, per-org counts (0 flags a possibly wrong handle),
    # and how many Videos still need the manual Reel check.
    by_org = Counter(r["Instagram_Handle"] for r in rows)     # posts per account
    fmt_counts = Counter(r["Post_Format"] for r in rows)      # posts per format
    reel_check_count = sum(1 for r in rows if r["Reel_Check_Needed"] == "YES")   # Videos to verify

    print(f"\n{'='*60}")
    print(f"Conversion complete.")
    print(f"  Posts in window:          {len(rows)}")
    print(f"  Skipped (outside window): {skipped_outside_window}")
    print(f"  Skipped (wrong org):      {skipped_wrong_org}")
    print(f"\nPost formats:")
    for k, v in sorted(fmt_counts.items()):
        print(f"  {k}: {v}")
    print(f"\nPosts per organization:")
    for org in TARGET_ORGS:
        n = by_org.get(org, 0)
        flag = " ⚠ 0 posts — verify handle" if n == 0 else ""
        print(f"  {org}: {n}{flag}")
    print(f"\n⚠ {reel_check_count} Video posts flagged Reel_Check_Needed=YES")
    print(f"  → Open each via Post_URL to verify Reel vs. Video before coding.\n")

    if warnings:
        print(f"Warnings ({len(warnings)}):")
        for w in warnings:
            print(w)

    print(f"\nOutput files:")
    print(f"  RAW:      {os.path.basename(raw_path)}")
    print(f"  CODER_A:  {os.path.basename(coder_path)}")
    print(f"\nNext steps:")
    print(f"  1. Check the 2 orgs with 0 posts (eduventuresnamibia, thinknamibia).")
    print(f"  2. Manually verify {reel_check_count} Video posts (Reel vs. Video).")
    print(f"  3. Open CODER_A template → code all posts → save as Study1_Content_Analysis_CODER_A.csv")
    print(f"  4. For reliability: give Coder B a random 20% stratified sample (~184 posts).")


if __name__ == "__main__":
    convert()
