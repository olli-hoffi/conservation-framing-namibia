# Data Dictionary

Every column of every dataset the analyses read, with its type, its range or its
allowed values, how many cells are missing, and what it means. Every type, range
and missing count was read off the data files themselves.

## Naming conventions

Most column names follow one of five prefixes. Knowing them makes the wide tables
readable without reading every row of this document.

| Prefix | Meaning |
|---|---|
| `f_` | binary message-frame code from the multimodal LLM pass, 1 present, 0 absent |
| `m_` | categorical code from the same pass, for example emotional valence or story structure |
| `MANUAL_` | human code, from the verified 101-post review or the blind 184-post pass |

Reliability between the `f_`/`m_` codes and the two human standards is Table 1 of
the thesis, produced by `analysis/01_study1/02_reliability.R`.


## `data/study1/analysis_base.csv`

920 rows, 65 columns.

| Column | Type | Range or values | Missing | Meaning |
|---|---|---|---|---|
| `Post_ID` | text | 920 distinct values | 0 | Instagram shortcode, the post's unique key across every file |
| `Actor_Name` | text | 19 distinct values | 0 | organization that published the post |
| `Actor_Type` | integer | 1 / 2 / 3 / 4 / 5 / 6 | 0 | organizational category, one of six (NGO, government, research, tourism, youth, network) |
| `Handle` | text | 19 distinct values | 0 | organization's Instagram handle |
| `Post_Format` | categorical | Carousel / Image / Video | 0 | Image, Carousel or Video |
| `Posting_Date` | text | 178 distinct values | 0 | date the post was published |
| `Likes` | integer | -1 to 214391 | 0 | like count at scrape time, -1 where the account hides likes |
| `Comments` | integer | 0 to 2059 | 0 | comment count reported by the platform |
| `Followers` | integer | 427 to 236580 | 0 | organization's follower count at scrape time |
| `eng_rate` | numeric | -0.002342 to 0.9062 | 0 | (likes + 1) / followers, the likes-based engagement rate |
| `cap_chars` | integer | 0 to 2192 | 0 | caption length in characters |
| `cap_words` | integer | 0 to 353 | 0 | caption length in words |
| `n_hashtags` | integer | 0 to 18 | 0 | hashtags in the caption |
| `n_mentions` | integer | 0 to 18 | 0 | @mentions in the caption |
| `n_emojis` | integer | 0 to 14 | 0 | emoji in the caption |
| `has_link` | binary | 0 / 1 | 0 | 1 if the caption contains a URL |
| `has_question` | binary | 0 / 1 | 0 | 1 if the caption contains a question mark |
| `clip_align` | numeric | 0.1435 to 0.4232 | 10 | CLIP cosine similarity between the caption and the display image |
| `n_comments_scraped` | integer | 0 to 15 | 0 | comments actually retrieved, capped at 15 by the collector |
| `comment_sent_mean` | numeric | -0.8015 to 0.9484 | 482 | mean VADER sentiment over the retrieved comments |
| `coded` | binary | 0 / 1 | 0 | 1 if the post carries multimodal LLM codes and enters the analyses |
| `f_Empathy` | binary | 0 / 1 | 3 | binary message-frame code from the multimodal LLM pass, 1 present, 0 absent (Empathy) |
| `f_Threat` | binary | 0 / 1 | 3 | binary message-frame code from the multimodal LLM pass, 1 present, 0 absent (Threat) |
| `f_Efficacy` | binary | 0 / 1 | 3 | binary message-frame code from the multimodal LLM pass, 1 present, 0 absent (Efficacy) |
| `f_Collective_ID` | binary | 0 / 1 | 3 | binary message-frame code from the multimodal LLM pass, 1 present, 0 absent (Collective ID) |
| `f_Normative` | binary | 0 / 1 | 3 | binary message-frame code from the multimodal LLM pass, 1 present, 0 absent (Normative) |
| `f_Moral` | binary | 0 / 1 | 3 | binary message-frame code from the multimodal LLM pass, 1 present, 0 absent (Moral) |
| `f_Economic` | binary | 0 / 1 | 3 | binary message-frame code from the multimodal LLM pass, 1 present, 0 absent (Economic) |
| `f_Scientific` | binary | 0 / 1 | 3 | binary message-frame code from the multimodal LLM pass, 1 present, 0 absent (Scientific) |
| `f_Youth_Addressed` | binary | 0 / 1 | 3 | binary message-frame code from the multimodal LLM pass, 1 present, 0 absent (Youth Addressed) |
| `f_Interactivity` | binary | 0 / 1 | 3 | binary message-frame code from the multimodal LLM pass, 1 present, 0 absent (Interactivity) |
| `f_Youth_Style` | binary | 0 / 1 | 38 | binary message-frame code from the multimodal LLM pass, 1 present, 0 absent (Youth Style) |
| `f_Protagonist` | binary | 0 / 1 | 39 | binary message-frame code from the multimodal LLM pass, 1 present, 0 absent (Protagonist) |
| `f_Onscreen_Text` | binary | 0 / 1 | 41 | binary message-frame code from the multimodal LLM pass, 1 present, 0 absent (Onscreen Text) |
| `f_Data_Visual` | binary | 0 / 1 | 39 | binary message-frame code from the multimodal LLM pass, 1 present, 0 absent (Data Visual) |
| `m_Emotional_Valence` | categorical | Distress / Hope / Mixed / Neutral | 3 | categorical manual-scheme code from the multimodal LLM pass (Emotional Valence) |
| `m_Story_Structure` | categorical | None / Problem / Problem-Solution / Solution | 3 | categorical manual-scheme code from the multimodal LLM pass (Story Structure) |
| `m_Primary_CTA` | categorical | Donate / Event / Learn / None / Share / Volunteer | 3 | categorical manual-scheme code from the multimodal LLM pass (Primary CTA) |
| `m_Primary_Topic` | text | 11 distinct values | 4 | categorical manual-scheme code from the multimodal LLM pass (Primary Topic) |
| `m_Language` | categorical | English / Mixed | 3 | categorical manual-scheme code from the multimodal LLM pass (Language) |
| `BERTopic` | integer | -99 to 33 | 0 | topic id assigned by the BERTopic model, -1 marks an outlier |

## `data/study1/comments_text.csv`

2,172 rows, 4 columns.

| Column | Type | Range or values | Missing | Meaning |
|---|---|---|---|---|
| `comment_id` | integer | 17842017765630756 to 18601041715055348 | 0 | unique key of the comment |
| `Post_ID` | text | 438 distinct values | 0 | Instagram shortcode, the post's unique key across every file |
| `commenter` | text | 1225 distinct values | 0 | pseudonym of the commenting account, C0001 to C1271 |
| `text` | text | 1549 distinct values | 32 | comment text as scraped |

## `data/study1/Study1_BlindCoded_184.csv`

184 rows, 25 columns.

| Column | Type | Range or values | Missing | Meaning |
|---|---|---|---|---|
| `Post_ID` | text | 184 distinct values | 0 | Instagram shortcode, the post's unique key across every file |
| `Post_URL` | text | 184 distinct values | 0 | public permalink of the post |
| `Actor_Name` | text | 19 distinct values | 0 | organization that published the post |
| `Post_Format` | categorical | Carousel / Image / Video | 0 | Image, Carousel or Video |
| `Posting_Date` | text | 113 distinct values | 0 | date the post was published |
| `Caption_Text` | text | 184 distinct values | 0 | the post caption as scraped |
| `MANUAL_Language` | categorical | Afrikaans / English | 2 | human code from the validation or blind coding pass (Language) |
| `MANUAL_Primary_Topic` | text | 10 distinct values | 1 | human code from the validation or blind coding pass (Primary Topic) |
| `MANUAL_Empathy` | binary | 0 / 1 | 2 | human code from the validation or blind coding pass (Empathy) |
| `MANUAL_Threat` | binary | 0 / 1 | 1 | human code from the validation or blind coding pass (Threat) |
| `MANUAL_Efficacy` | binary | 0 / 1 | 0 | human code from the validation or blind coding pass (Efficacy) |
| `MANUAL_Collective_ID` | binary | 0 / 1 | 0 | human code from the validation or blind coding pass (Collective ID) |
| `MANUAL_Normative` | binary | 0 / 1 | 3 | human code from the validation or blind coding pass (Normative) |
| `MANUAL_Moral` | binary | 0 / 1 | 2 | human code from the validation or blind coding pass (Moral) |
| `MANUAL_Economic` | binary | 0 / 1 | 3 | human code from the validation or blind coding pass (Economic) |
| `MANUAL_Scientific` | binary | 0 / 1 | 2 | human code from the validation or blind coding pass (Scientific) |
| `MANUAL_Primary_CTA` | categorical | Donate / Event / Learn / None / Volunteer | 0 | human code from the validation or blind coding pass (Primary CTA) |
| `MANUAL_Youth_Addressed` | binary | 0 / 1 | 3 | human code from the validation or blind coding pass (Youth Addressed) |
| `MANUAL_Youth_Style` | binary | 0 / 1 | 2 | human code from the validation or blind coding pass (Youth Style) |
| `MANUAL_Interactivity` | binary | 0 / 1 | 3 | human code from the validation or blind coding pass (Interactivity) |
| `MANUAL_Protagonist` | binary | 0 / 1 | 1 | human code from the validation or blind coding pass (Protagonist) |
| `MANUAL_Emotional_Valence` | categorical | Distress / Hope / Mixed / Neutral | 1 | human code from the validation or blind coding pass (Emotional Valence) |
| `MANUAL_Story_Structure` | categorical | None / Problem / Problem-Solution / Solution | 1 | human code from the validation or blind coding pass (Story Structure) |
| `MANUAL_Onscreen_Text` | binary | 0 / 1 | 1 | human code from the validation or blind coding pass (Onscreen Text) |
| `MANUAL_Data_Visual` | binary | 0 / 1 | 2 | human code from the validation or blind coding pass (Data Visual) |

## `data/study1/Study1_LLM_multimodal_920.csv`

921 rows, 1 columns.

| Column | Type | Range or values | Missing | Meaning |
|---|---|---|---|---|
| `# multimodal LLM TEST · model=sonnet(vision) · display-image only · 920 posts · 917 with image` | text | 921 distinct values | 0 |  |

## `data/study1/Study1_LLM_multimodal_reviewed.csv`

109 rows, 27 columns.

| Column | Type | Range or values | Missing | Meaning |
|---|---|---|---|---|
| `Post_ID` | text | 109 distinct values | 0 | Instagram shortcode, the post's unique key across every file |
| `Post_URL` | text | 102 distinct values | 6 | public permalink of the post |
| `Actor_Name` | text | 14 distinct values | 6 | organization that published the post |
| `Post_Format` | categorical | 0 / Carousel / Image / Video | 6 | Image, Carousel or Video |
| `Posting_Date` | text | 67 distinct values | 6 | date the post was published |
| `Caption_Text` | text | 101 distinct values | 8 | the post caption as scraped |
| `reviewed` | binary | 0 / 1 | 8 |  |
| `changed` | binary | 0 / 1 | 8 |  |
| `MANUAL_Empathy` | binary | 0 / 1 | 10 | human code from the validation or blind coding pass (Empathy) |
| `MANUAL_Threat` | binary | 0 / 1 | 10 | human code from the validation or blind coding pass (Threat) |
| `MANUAL_Efficacy` | binary | 0 / 1 | 10 | human code from the validation or blind coding pass (Efficacy) |
| `MANUAL_Collective_ID` | binary | 0 / 1 | 10 | human code from the validation or blind coding pass (Collective ID) |
| `MANUAL_Normative` | binary | 0 / 1 | 10 | human code from the validation or blind coding pass (Normative) |
| `MANUAL_Moral` | categorical | 0 / 1 / Hope | 10 | human code from the validation or blind coding pass (Moral) |
| `MANUAL_Economic` | categorical | 0 / 1 / None / Solution | 10 | human code from the validation or blind coding pass (Economic) |
| `MANUAL_Scientific` | categorical | 0 / 1 / None | 10 | human code from the validation or blind coding pass (Scientific) |
| `MANUAL_Youth_Addressed` | categorical | 0 / 1 / Wildlife | 10 | human code from the validation or blind coding pass (Youth Addressed) |
| `MANUAL_Interactivity` | categorical | 0 / 1 / English | 10 | human code from the validation or blind coding pass (Interactivity) |
| `MANUAL_Emotional_Valence` | categorical | 0 / Hope / Mixed / Neutral | 10 | human code from the validation or blind coding pass (Emotional Valence) |
| `MANUAL_Story_Structure` | categorical | 1 / None / Problem / Problem-Solution / Solution | 10 | human code from the validation or blind coding pass (Story Structure) |
| `MANUAL_Primary_CTA` | categorical | 1 / Donate / Event / Learn / None / Share | 10 | human code from the validation or blind coding pass (Primary CTA) |
| `MANUAL_Primary_Topic` | text | 11 distinct values | 11 | human code from the validation or blind coding pass (Primary Topic) |
| `MANUAL_Language` | categorical | Afrikaans / English / Mixed | 12 | human code from the validation or blind coding pass (Language) |
| `MANUAL_Youth_Style` | binary | 0 / 1 | 15 | human code from the validation or blind coding pass (Youth Style) |
| `MANUAL_Protagonist` | binary | 0 / 1 | 15 | human code from the validation or blind coding pass (Protagonist) |
| `MANUAL_Onscreen_Text` | binary | 0 / 1 | 14 | human code from the validation or blind coding pass (Onscreen Text) |
| `MANUAL_Data_Visual` | binary | 0 / 1 | 15 | human code from the validation or blind coding pass (Data Visual) |

## `data/study1/sample_150.csv`

150 rows, 22 columns.

| Column | Type | Range or values | Missing | Meaning |
|---|---|---|---|---|
| `Post_ID` | text | 150 distinct values | 0 | Instagram shortcode, the post's unique key across every file |
| `Actor_Name` | text | 19 distinct values | 0 | organization that published the post |
| `Post_Format` | categorical | Carousel / Image / Video | 0 | Image, Carousel or Video |
| `BERTopic` | integer | -1 to 33 | 0 | topic id assigned by the BERTopic model, -1 marks an outlier |
| `functional_genre` | text | 13 distinct values | 0 |  |
| `blindspot` | categorical | False / True | 0 |  |
| `comment_avail` | categorical | False / True | 0 |  |
| `n_comments_scraped` | integer | 0 to 15 | 0 | comments actually retrieved, capped at 15 by the collector |
| `Comments` | integer | 0 to 2059 | 0 | comment count reported by the platform |
| `eng_rate` | numeric | 0.000679 to 0.9062 | 0 | (likes + 1) / followers, the likes-based engagement rate |
| `pca1_bin` | categorical | hi / lo / mid | 0 |  |
| `pca2_bin` | categorical | hi / lo / mid | 0 |  |
| `pca3_bin` | categorical | hi / lo / mid | 0 |  |
| `commemoration_day` | categorical | False / True | 0 |  |
| `live_series` | categorical | False / True | 0 |  |
| `hiring` | categorical | False / True | 0 |  |
| `pure_info` | categorical | False / True | 0 |  |
| `visual_zeroshot` | categorical | animal_portrait / behind_the_scenes / branding_logo / infographic_data / landscape_scenery / people_event / person_portrait / poster_text | 0 |  |
| `visual_hdbscan` | integer | -1 to 19 | 0 |  |
| `selection_reason` | categorical | blindspot / coverage_guarantee / genre_core / high_discussion | 0 |  |
| `image_path` | text | 150 distinct values | 0 |  |
| `image_exists` | categorical | True | 0 |  |

## `data/processed/meta_ads_clean.csv`

24 rows, 19 columns.

| Column | Type | Range or values | Missing | Meaning |
|---|---|---|---|---|
| `round` | integer | 1 / 2 | 0 | campaign round, 1 or 2 |
| `condition` | categorical | Empathy / Narrative / Neutral / Social Norm | 0 | framing condition, Neutral, Empathy, Social Norm or Narrative |
| `variant` | integer | 1 / 2 / 3 | 0 | hook variant 1 to 3, the same three clips serve all four conditions |
| `ad_name` | text | 12 distinct values | 0 |  |
| `impressions` | integer | 15093 to 21042 | 0 | times the advertisement was displayed |
| `reach` | integer | 14384 to 18960 | 0 | unique accounts that saw the advertisement |
| `frequency` | numeric | 1.017 to 1.154 | 0 |  |
| `spend` | numeric | 9.82 to 9.93 | 0 |  |
| `clicks` | integer | 78 to 171 | 0 | all clicks including non-link interactions |
| `link_clicks` | integer | 50 to 124 | 0 | clicks on the advertisement's link |
| `ctr` | numeric | 0.4078 to 0.9944 | 0 |  |
| `link_ctr` | numeric | 0.2614 to 0.7088 | 0 |  |
| `reactions` | integer | 147 to 230 | 0 | likes and other reactions |
| `comments` | integer | 0 / 1 / 2 / 3 / 4 / 5 / 6 / 10 | 0 |  |
| `shares` | integer | 6 to 22 | 0 | shares |
| `saves` | integer | 0 / 1 / 2 / 3 / 4 / 5 / 7 / 8 | 0 | saves |
| `video_3s_views` | integer | 2728 to 4593 | 0 | views that reached three seconds |
| `thruplays` | integer | 802 to 1332 | 0 |  |
| `video_p100` | integer | 125 to 301 | 0 |  |

## `data/processed/ga4_clean.csv`

35 rows, 11 columns.

| Column | Type | Range or values | Missing | Meaning |
|---|---|---|---|---|
| `round` | integer | 1 / 2 | 0 | campaign round, 1 or 2 |
| `condition` | categorical | Empathy / Narrative / Neutral / Social Norm | 0 | framing condition, Neutral, Empathy, Social Norm or Narrative |
| `variant` | integer | 1 / 2 / 3 | 0 | hook variant 1 to 3, the same three clips serve all four conditions |
| `utm_content` | text | 11 distinct values | 0 | UTM tag, condition and variant, the link between advertisement and session |
| `country` | categorical | Cameroon / Germany / Laos / Namibia / South Africa | 0 |  |
| `sessions` | integer | 1 / 2 / 3 / 4 / 5 | 0 | landing-page sessions in this cell |
| `avg_session_duration_s` | numeric | 0 to 342 | 0 | mean session duration in the cell, in seconds |
| `avg_engagement_time_s` | numeric | 0 to 136 | 0 |  |
| `engagement_rate` | numeric | 0 to 1 | 0 |  |
| `first_visits` | empty |  | 35 |  |
| `user_engagement_s` | integer | 0 to 276 | 0 |  |

## `data/processed/exitsurvey_clean.csv`

13 rows, 13 columns.

| Column | Type | Range or values | Missing | Meaning |
|---|---|---|---|---|
| `case` | integer | 19 to 82 | 0 |  |
| `ref` | categorical | empathy_2 / empathy_3 / narrative_1 / narrative_2 / neutral_2 / social_norm_1 / social_norm_2 / social_norm_3 | 0 |  |
| `condition` | categorical | Empathy / Narrative / Neutral / Social Norm | 0 | framing condition, Neutral, Empathy, Social Norm or Narrative |
| `variant` | integer | 1 / 2 / 3 | 0 | hook variant 1 to 3, the same three clips serve all four conditions |
| `started` | text | 13 distinct values | 0 |  |
| `round` | integer | 1 / 2 | 0 | campaign round, 1 or 2 |
| `gender` | integer | 1 / 2 | 0 |  |
| `age_group` | integer | 1 / 2 / 3 | 0 |  |
| `emp_state` | integer | 4 / 5 | 0 |  |
| `ident_state` | integer | 3 / 4 / 5 | 0 |  |
| `env_concern` | integer | 2 / 4 / 5 | 0 |  |
| `bio_water` | integer | 1 / 4 / 5 | 0 |  |
| `collectivism` | integer | 1 / 4 / 5 | 0 |  |

## `data/processed/pretest_composites.csv`

480 rows, 5 columns.

| Column | Type | Range or values | Missing | Meaning |
|---|---|---|---|---|
| `id` | integer | 11 to 57 | 0 |  |
| `condition` | categorical | Empathy / Narrative / Neutral / Social Norm | 0 | framing condition, Neutral, Empathy, Social Norm or Narrative |
| `scale` | categorical | CTRL1 / CTRL2 / EMP / NAR / NORM | 0 |  |
| `score` | numeric | 1 to 5 | 0 |  |
| `n_items` | binary | 0 / 1 | 0 |  |

## `data/processed/pretest_items_long.csv`

1,056 rows, 6 columns.

| Column | Type | Range or values | Missing | Meaning |
|---|---|---|---|---|
| `id` | integer | 11 to 57 | 0 |  |
| `condition` | categorical | Empathy / Narrative / Neutral / Social Norm | 0 | framing condition, Neutral, Empathy, Social Norm or Narrative |
| `scale` | categorical | CTRL1 / CTRL2 / EMP / NAR / NORM | 0 |  |
| `item_num` | integer | 1 to 11 | 0 |  |
| `item_id` | text | 11 distinct values | 0 |  |
| `score` | integer | 1 / 2 / 3 / 4 / 5 | 0 |  |

## `data/processed/pretest_demographics.csv`

24 rows, 4 columns.

| Column | Type | Range or values | Missing | Meaning |
|---|---|---|---|---|
| `id` | integer | 11 to 57 | 0 |  |
| `age_group` | categorical | 18–24 / 25–29 / 36 or older | 0 |  |
| `gender` | categorical | Man / Woman | 0 |  |
| `english_prof` | categorical | Advanced / Fluent / Native / Intermediate | 0 |  |

## `data/processed/pretest_order.csv`

96 rows, 3 columns.

| Column | Type | Range or values | Missing | Meaning |
|---|---|---|---|---|
| `id` | integer | 11 to 57 | 0 |  |
| `position` | integer | 1 / 2 / 3 / 4 | 0 |  |
| `page_label` | integer | 1 / 2 / 3 / 4 | 0 |  |

## `data/study1/features/`

Feature tables produced by the Python pipeline and read by the Study 1 scripts.
They share the keys above, `Post_ID` for post-level tables, `Actor_Name` for
organization-level ones and `commenter` for audience ones.

| File | Rows | Columns | Key |
|---|---|---|---|
| `attn_org_centrality.csv` | 9 | 5 | — |
| `attn_places.csv` | 41 | 5 | — |
| `attn_species_threat.csv` | 29 | 6 | — |
| `blindspots.csv` | 143 | 10 | Post_ID |
| `caption_emoji_top.csv` | 225 | 3 | — |
| `caption_features.csv` | 917 | 51 | Post_ID |
| `caption_hashtag_top.csv` | 822 | 3 | — |
| `caption_hashtags_long.csv` | 2,621 | 3 | Post_ID |
| `codebook_frame_prevalence.csv` | 14 | 7 | — |
| `comment_topics_info.csv` | 32 | 5 | — |
| `commenter_org_edges.csv` | 1,252 | 3 | commenter |
| `commenter_summary.csv` | 1,225 | 6 | commenter |
| `comments_labeled.csv` | 2,172 | 12 | comment_id |
| `eng_comments_nb_irr.csv` | 9 | 4 | — |
| `eng_frame_discussion_cliff.csv` | 13 | 5 | — |
| `entities_mentions_edges.csv` | 377 | 4 | Post_ID |
| `entities_ner_orgs_long.csv` | 1,718 | 3 | Post_ID |
| `entities_org_edges.csv` | 183 | 4 | src_org |
| `entities_place_summary.csv` | 317 | 5 | — |
| `entities_places_long.csv` | 1,184 | 5 | Post_ID |
| `entities_species_long.csv` | 426 | 5 | Post_ID |
| `entities_species_summary.csv` | 29 | 6 | — |
| `image_features.csv` | 911 | 16 | Post_ID |
| `image_near_duplicates.csv` | 64 | 4 | — |
| `llm_comment_stance.csv` | 314 | 5 | — |
| `llm_post_features.csv` | 150 | 29 | — |
| `misuse_engagement_spearman.csv` | 12 | 3 | — |
| `misuse_mixed_model.csv` | 9 | 5 | — |
| `msg_frame_cooccurrence.csv` | 91 | 3 | — |
| `msg_mfd_ci.csv` | 5 | 4 | — |
| `msg_nrc_ci.csv` | 8 | 4 | — |
| `msg_voice_ci.csv` | 4 | 4 | — |
| `org_actortype_profile.csv` | 6 | 7 | — |
| `org_cluster_profiles.csv` | 4 | 7 | — |
| `org_clusters.csv` | 16 | 18 | Actor_Name |
| `org_content_mix.csv` | 100 | 4 | Actor_Name |
| `org_feature_matrix.csv` | 19 | 52 | Actor_Name |
| `org_shared_audience_edges.csv` | 19 | 3 | org_a |
| `reliability_kappa.csv` | 19 | 10 | — |
| `sampling_frame.csv` | 917 | 19 | Post_ID |
| `tax_comment_stance.csv` | 11 | 3 | — |
| `tax_species_congruence.csv` | 5 | 3 | — |
| `temporal_dow.csv` | 7 | 4 | — |
| `temporal_events.csv` | 13 | 4 | — |
| `temporal_monthly.csv` | 6 | 8 | — |
| `temporal_org_cadence.csv` | 19 | 10 | Actor_Name |
| `temporal_post_features.csv` | 917 | 16 | Post_ID |
| `visual_clusters.csv` | 917 | 4 | Post_ID |
