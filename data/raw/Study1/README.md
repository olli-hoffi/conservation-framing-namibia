# Study 1 raw inputs

What the Study 1 pipeline starts from, before any feature is computed.

## The corpus

`Study1_Posts_RAW_20260520.csv` holds all 920 posts as they were retrieved on
20 May 2026, one row per post with its organization, format, date, like and
comment counts, and the caption. It is semicolon-separated because captions
contain commas.

It is read by `analysis/01_study1/01_sampling_frame.R`, which builds Figure 2,
and by the caption and entity feature scripts under `python/study1/4_features/`,
which take the `Post_ID` and `Caption_Text` columns from it.

The captions are reproduced as retrieved, with one exception. Where a caption
tagged a private account, the handle was replaced by the same pseudonym the
account carries everywhere else in the package. Organization accounts keep their
names, since they are the published subject of the study.

## The organization accounts

`Study1_Org_Profiles_2026-07-03.json` holds the account-level snapshot of the 19
organizations taken on 3 July 2026, with follower and following counts, lifetime
post count, highlight count, and the business and verified flags. That is every
field `python/study1/4_features/c1_org_features.py` reads, and nothing else.

The Apify profile scrape it was cut from also returned 382 unrelated accounts
Instagram suggests as related, each with a name and a profile picture, and the
last twelve posts of every organization with the accounts tagged in them. None of
that entered any analysis and none of it is shipped.

## The sampling frame

The four `Study1_Organization_Identification_Workflow-*.csv` files document how
the 19 organizations were found and reduced, from the raw pool through directory
screening to the deduplicated master list, plus the search protocol that produced
them. They are the record of the sampling decision, not an input to any script.

## What is not here

The raw Apify export the corpus was built from is not shipped. It carries the
account name and profile picture of every commenter, and the package
pseudonymises those throughout. `python/study1/1_collect/convert_apify_to_raw.py`
documents the conversion that produced the corpus file above.
