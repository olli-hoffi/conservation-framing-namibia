# Raw data

Everything the analysis scripts read before any processing step. Five folders:

| Folder | Contents | Read by |
|---|---|---|
| `Study1/` | The Instagram corpus, the multimodal LLM codes, and the blind validation coding | `python/study1/4_features/*`, then the Study 1 R scripts via `data/study1/` |
| `MetaAds/` | Per-advertisement and per-round exports from Meta Ads Manager, plus the ad-comment corpus | eight scripts under `analysis/02_study2/`, from `03_process_campaign.R` to `14_exploratory.R` |
| `GA4/` | Google Analytics 4 session exports for both campaign rounds | `01b_process_study2.R` |
| `ExitSurvey/` | The SoSci exit-survey export | `01b_process_study2.R` |
| `Pretest/` | The SoSci manipulation-check export | `02_study2/01_process_pretest.R` |

### How the advertisement comments were obtained

All 71 comments on the 24 advertisement placements are in
`MetaAds/MetaAds_comments_2026-07-09.csv`. Incoming comments were hidden under a
standing protocol during the campaign, so at retrieval 52 were still hidden and the
Graph API did not return them. Those were read out of Meta Business Suite instead.
The file therefore carries a `source` column recording which route each comment
came through. Usernames and permalinks were removed before publication, and the
comment text is pseudonymised.

The joined table `output/tables/study2_comment_coded_review.csv` pairs all 71
records and every available comment text with their condition and resolved stance,
language, and local-authority codes. It contains no usernames or permalinks.

`GA4/_superseded/` holds the earlier export that the final one replaced, so the switch can be
traced. `GA4/_reference/` holds the property configuration.

Processed derivatives are written to `data/processed/` and `data/study1/`, never back into
this folder. Run the scripts from the package root, `here()` anchors on it.
