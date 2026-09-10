#!/usr/bin/env python3
"""
Phase C - Themes 1, 3, 4 (data-generation). Temporal structure of the corpus:
per-post calendar features, per-org posting cadence, monthly volume + format
adoption, and alignment to an environmental commemoration-day calendar.

Post timestamps are DATE-ONLY (no time component), so time-of-day analysis is not
possible for posts; day-of-week, week, month, and event alignment are. Comment
timestamps carry a time (handled in c6_comment_features.py).

Reads : analysis_base.csv
Writes: output/temporal_post_features.csv, output/temporal_org_cadence.csv,
        output/temporal_monthly.csv, output/temporal_dow.csv, output/temporal_events.csv
Run   : "../4. NLP Pipeline/.venv/bin/python" c2_temporal_features.py
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
# WHY temporal features: conservation communication is often calendar-driven
# (awareness days, campaigns, seasons). These features let the thesis ask whether
# Namibian orgs post reactively around global observances, whether they post on a
# regular cadence or in bursts, and whether video adoption grew over the window.
import os, calendar          # calendar: month lengths + weekday maths for the floating observances
import numpy as np
import pandas as pd
from datetime import date   # date objects for the commemoration-day calendar

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

OUT = str(_FEAT)
os.makedirs(OUT, exist_ok=True)
DOW = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]   # index 0-6 -> weekday label (Python's weekday(): Mon=0)

# ---- commemoration-day calendar ----------------------------------------------
# Environmental / conservation observances. Fixed-date entries plus a few floating
# ones (nth or last weekday) computed per corpus year. Reusable across years.
# Purpose: measure whether posts cluster around these days (a "hook the calendar"
# communication strategy that is common in NGO social media).
FIXED = {  # (month, day): name
    (2, 2): "World Wetlands Day", (3, 3): "World Wildlife Day",
    (3, 20): "World Rewilding Day", (3, 21): "Int'l Day of Forests",
    (3, 22): "World Water Day", (3, 23): "World Meteorological Day",
    (4, 22): "Earth Day", (4, 25): "World Penguin Day",
    (5, 22): "Int'l Day for Biological Diversity", (6, 5): "World Environment Day",
    (6, 8): "World Ocean Day", (6, 21): "World Giraffe Day",
    (7, 28): "World Nature Conservation Day", (8, 10): "World Lion Day",
    (8, 12): "World Elephant Day", (9, 22): "World Rhino Day",
    (11, 21): "World Fisheries Day", (12, 4): "Wildlife Conservation Day / Int'l Cheetah Day",
    (12, 5): "World Soil Day",
    # NOTE (review): (3, 21) is set twice in this literal - "Int'l Day of Forests"
    # (above) and "Namibia Independence Day" (here). Python keeps only the LAST value,
    # so 21 March resolves to Independence Day and Forests is silently lost from the
    # calendar. Both fall on the same date; flagging, not fixing (comments-only pass).
    (3, 21): "Namibia Independence Day",  # national anchor (co-located with Forests)
}
# nth occurrence of a weekday in a month (e.g. 3rd Saturday of February).
# weekday uses Mon=0 convention. Used for observances defined as "the Nth <weekday>".
def nth_weekday(year, month, weekday, n):  # weekday: Mon=0
    d = date(year, month, 1)                       # start at the 1st of the month
    off = (weekday - d.weekday()) % 7              # days forward to the FIRST target weekday
    return date(year, month, 1 + off + 7 * (n - 1))  # then step n-1 whole weeks on
# last occurrence of a weekday in a month (e.g. Earth Hour = last Saturday of March).
def last_weekday(year, month, weekday):
    last = calendar.monthrange(year, month)[1]     # number of days in the month
    d = date(year, month, last)                    # start at the last day
    return date(year, month, last - ((d.weekday() - weekday) % 7))  # walk back to the target weekday

# Build the full observance calendar {date: name} for one year: fixed dates plus the
# four floating observances resolved to that year's actual date.
def calendar_for(year):
    cal = {}
    for (m, dd), name in FIXED.items():
        cal[date(year, m, dd)] = name
    cal[nth_weekday(year, 2, 5, 3)] = "World Pangolin Day"      # 3rd Sat Feb
    cal[last_weekday(year, 3, 5)] = "Earth Hour"                # last Sat Mar
    cal[nth_weekday(year, 5, 4, 3)] = "Endangered Species Day"  # 3rd Fri May
    cal[nth_weekday(year, 9, 5, 1)] = "Int'l Vulture Awareness Day"  # 1st Sat Sep
    return cal

# ---- load ---------------------------------------------------------------------
df = pd.read_csv(str(_BASE), sep=";", encoding="utf-8-sig")
d = df[df["coded"] == 1].copy()                    # analysis restricted to coded posts
d["dt"] = pd.to_datetime(d["Posting_Date"], errors="coerce")   # parse the date (bad values -> NaT)
d = d.dropna(subset=["dt"])                        # drop posts with an unparseable date (they cannot be placed on the calendar)
years = sorted(d["dt"].dt.year.unique())           # which calendar years the corpus actually spans
# Merge the per-year observance calendars into one big {date: name} lookup covering
# every year present in the corpus.
CAL = {}
for y in years:
    CAL.update(calendar_for(y))
cal_dates = sorted(CAL)                             # sorted list of all observance dates (for nearest-event search)
print(f"Calendar: {len(cal_dates)} observances across years {years}")

# ---- per-post features --------------------------------------------------------
# For a given post timestamp, find the CLOSEST observance and how many days off it is.
# delta < 0 means the post came BEFORE the event (a teaser); delta > 0 means after.
def nearest_event(ts):
    dd = ts.date()
    best = min(cal_dates, key=lambda e: abs((e - dd).days))   # observance minimising absolute day-distance
    delta = (dd - best).days   # negative = posted before the event
    return CAL[best], best, delta

# Build one calendar-feature record per post.
recs = []
for _, r in d.iterrows():
    ts = r["dt"]
    ev, evd, delta = nearest_event(ts)             # nearest observance name, its date, signed day-gap
    recs.append({
        "Post_ID": r["Post_ID"], "Actor_Name": r["Actor_Name"], "Handle": str(r["Handle"]).lower(),
        "date": ts.date().isoformat(), "year": ts.year, "month": ts.month,
        "iso_week": int(ts.isocalendar().week), "dow": ts.weekday(), "dow_name": DOW[ts.weekday()],   # ISO week number + weekday index/name
        "is_weekend": int(ts.weekday() >= 5), "day_of_year": ts.dayofyear,   # Sat/Sun flag; ordinal day 1-366
        "nearest_event": ev, "event_date": evd.isoformat(), "days_to_event": delta,
        "on_event": int(abs(delta) <= 1), "event_window3": int(abs(delta) <= 3),   # posted within +/-1 day / +/-3 days of an observance
    })
post = pd.DataFrame(recs)
post.to_csv(os.path.join(OUT, "temporal_post_features.csv"), index=False)

# ---- per-org cadence ----------------------------------------------------------
# One row per org describing HOW REGULARLY it posts. Join back to d for Post_Format,
# then group by org.
org = []
for (name, handle), g in post.merge(d[["Post_ID", "Post_Format"]], on="Post_ID").groupby(["Actor_Name", "Handle"]):
    dts = pd.to_datetime(g["date"]).sort_values()  # this org's post dates, chronological
    gaps = dts.diff().dt.days.dropna()             # day-gaps between consecutive posts (first diff is NaN -> dropped)
    span = (dts.max() - dts.min()).days            # active window length in days
    org.append({
        "Actor_Name": name, "Handle": handle, "n_posts": len(g),
        "span_days": span, "active_days": dts.dt.date.nunique(),   # distinct days posted on
        "posts_per_week": round(len(g) / (span / 7), 2) if span > 0 else np.nan,   # average posting frequency (guarded)
        "median_gap_days": float(gaps.median()) if len(gaps) else np.nan,   # typical spacing between posts (bursty vs steady)
        "pct_weekend": round(g["is_weekend"].mean(), 3),   # share of posts on Sat/Sun
        "top_dow": DOW[int(g["dow"].mode().iloc[0])],      # the org's single most common posting weekday
        "on_event_rate": round(g["on_event"].mean(), 3),   # share of posts landing on an observance day
    })
org_df = pd.DataFrame(org).sort_values("posts_per_week", ascending=False)
org_df.to_csv(os.path.join(OUT, "temporal_org_cadence.csv"), index=False)

# ---- monthly volume + format adoption -----------------------------------------
# One row per calendar month: how many posts and the mix of formats. Lets the thesis
# show, e.g., video adoption rising over the sampling window.
# WHY the decomposition columns: the corpus-wide video share roughly doubles over
# the window, but that rise can come from two distinct processes - organizations
# switching formats (adoption) or video-native organizations posting more
# (composition). Splitting each month by the video-led accounts (majority video
# share over the whole window) separates the two readings.
m = d.assign(ym=d["dt"].dt.to_period("M").astype(str))   # add a "YYYY-MM" month key
org_video = m.groupby("Actor_Name")["Post_Format"].apply(lambda s: (s == "Video").mean())  # per-org video share, whole window
video_led = set(org_video[org_video > 0.5].index)         # majority-video accounts
m["is_video_led"] = m["Actor_Name"].isin(video_led)
monthly = m.groupby("ym").agg(
    n_posts=("Post_ID", "size"),                                        # posts that month
    pct_video=("Post_Format", lambda s: (s == "Video").mean()),         # share Video (0/1 mean = proportion)
    pct_carousel=("Post_Format", lambda s: (s == "Carousel").mean()),   # share Carousel
    pct_image=("Post_Format", lambda s: (s == "Image").mean()),         # share single Image
    eng_rate_med=("eng_rate", lambda s: pd.to_numeric(s, errors="coerce").median()),   # median engagement that month
    pct_posts_video_led=("is_video_led", "mean"),                       # share of the month's posts from video-led accounts
).reset_index()
pct_video_rest = (m[~m["is_video_led"]].groupby("ym")["Post_Format"]
                  .apply(lambda s: (s == "Video").mean())
                  .rename("pct_video_rest").reset_index())              # video share among the remaining organizations
monthly = monthly.merge(pct_video_rest, on="ym", how="left")
monthly.to_csv(os.path.join(OUT, "temporal_monthly.csv"), index=False)
print("video-led accounts (majority video share over the window):", sorted(video_led))

# ---- day-of-week distribution + engagement -----------------------------------
# Do posts (and their engagement) concentrate on particular weekdays?
dowt = post.merge(d[["Post_ID", "eng_rate"]], on="Post_ID")   # bring engagement onto the per-post calendar table
dowt["eng_rate"] = pd.to_numeric(dowt["eng_rate"], errors="coerce")
dow_tab = dowt.groupby(["dow", "dow_name"]).agg(
    n_posts=("Post_ID", "size"), eng_rate_med=("eng_rate", "median")).reset_index().sort_values("dow")   # ordered Mon->Sun via the numeric dow
dow_tab.to_csv(os.path.join(OUT, "temporal_dow.csv"), index=False)

# ---- event-response table -----------------------------------------------------
# Keep only posts that landed ON an observance (within +/-1 day), then count how many
# posts and how many distinct orgs reacted to each event = which days drive content.
ev = post[post["on_event"] == 1]
etab = (ev.groupby(["nearest_event", "event_date"])
        .agg(n_posts=("Post_ID", "size"), n_orgs=("Actor_Name", "nunique")).reset_index()
        .sort_values("n_posts", ascending=False))
etab.to_csv(os.path.join(OUT, "temporal_events.csv"), index=False)

# ---- audit --------------------------------------------------------------------
# Console summary: counts written, calendar-alignment rates, and previews of the four
# summary tables so the run can be sanity-checked at a glance.
print(f"\nc2_temporal_features.py: {len(post)} posts")
print("  per-post -> temporal_post_features.csv | org cadence -> temporal_org_cadence.csv")
print("  monthly -> temporal_monthly.csv | dow -> temporal_dow.csv | events -> temporal_events.csv")
print(f"\nPosts within +/-1 day of a commemoration day: {int(post['on_event'].sum())} "
      f"({100*post['on_event'].mean():.1f}%); within +/-3 days: "
      f"{int(post['event_window3'].sum())} ({100*post['event_window3'].mean():.1f}%)")
print("\nTop event responses (posts within 1 day):")
print(etab.head(12).to_string(index=False))
print("\nDay-of-week:")
print(dow_tab.assign(eng_rate_pct=lambda x: (100*x.eng_rate_med).round(2)).to_string(index=False))   # engagement as % for readability
print("\nMonthly volume + video share:")
print(monthly.assign(pct_video=lambda x:(100*x.pct_video).round(0)).to_string(index=False))
print("\nCadence extremes (posts/week):")
print(org_df[["Actor_Name","n_posts","posts_per_week","median_gap_days","top_dow","on_event_rate"]].head(6).to_string(index=False))
