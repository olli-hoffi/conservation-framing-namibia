# =============================================================================
# 01_study1/05_org_styles_genres.R
#
# WHERE THIS BELONGS
#   In the thesis: Chapter 3, Results: Organizational Styles, Genres, and Posting Rhythm
#   Produces: Figure 3, Table B3
#   Status: Exploratory. Study 1 carries no preregistration at all, so every
#   association here is descriptive and none of it tests a hypothesis that was
#   stated in advance.
#
# Study 1 (content analysis, PURELY EXPLORATORY, no preregistration): what
# conservation Instagram *is* as an organisational field.
#
# The UNIT OF ANALYSIS here is the ORGANISATION, not the post: each row is one
# account summarised over its posts. Three descriptive views:
#   1. Actor-type profile (headline features per type).
#   2. A data-driven communication-style typology (k-means / Ward on scaled
#      style features) of the higher-volume orgs.
#   3. Each org's content-mix portfolio (genre shares) and posting cadence.
#
# CAVEAT (actor-type contrasts are DESCRIPTIVE ONLY): the near-census sampling
# confounds four of the six actor types with a single organisation each (only
# the NGO and Umbrella types hold >1 org), so an actor-type gap cannot be
# separated from an individual-account effect. Read the profile as descriptive
# colour, never as a powered between-groups comparison.
#
# Input : data/study1/features/org_feature_matrix.csv   (one row per org)
#         data/study1/features/temporal_org_cadence.csv (posts/week per org)
#         data/study1/features/sampling_frame.csv       (post-level genre labels)
#         data/raw/Study1/Study1_Posts_RAW_20260520.csv  (caption series markers)
# Output: output/tables/org_actortype_profile.csv, org_clusters.csv,
#         org_cluster_profiles.csv, org_content_mix.csv, org_silhouette_scan.csv,
#         org_low_volume.csv, org_rhythm_video_share.csv, org_calendar_peaks.csv,
#         org_monthly_volume.csv, org_volume_summary.csv, org_cadence_by_span.csv,
#         org_cadence_summary.csv, org_audience_size.csv, org_retrieval_coverage.csv,
#         org_retrieval_coverage_summary.csv, org_ministry_cadence.csv,
#         org_recurring_formats.csv, org_recurring_format_summary.csv;
#         output/figures/fig_org_content_mix
# =============================================================================

# cluster supplies silhouette(); factoextra supplies the fviz_* dendrogram /
# cluster-scatter helpers; viridisLite supplies the colourblind-safe palette.
pacman::p_load(here, tidyverse, cluster, factoextra, viridisLite, flextable, officer)
list.files(here("src"), full.names = TRUE) %>% walk(source)

# k-means, the Ward tree and the bootstrap CIs all draw random numbers; fixing the
# seed makes every run reproduce the same partition and the same intervals.
set.seed(42)

org_features <- read_csv(here("data/study1/features/org_feature_matrix.csv"), show_col_types = FALSE)   # one row per org, style features
org_cadence  <- read_csv(here("data/study1/features/temporal_org_cadence.csv"), show_col_types = FALSE) # posts-per-week per org
post_genres  <- read_csv(here("data/study1/features/sampling_frame.csv"), show_col_types = FALSE)       # post-level genre labels
raw_posts    <- read_delim(here("data/raw/Study1/Study1_Posts_RAW_20260520.csv"), delim = ";",
                           show_col_types = FALSE, locale = locale(encoding = "UTF-8"))                 # captions + dates

nrow(org_features)
# 19 organisations, the units every org-level descriptive is computed on.


# ---- 1. Actor-type profile ---------------------------------------------------
# Plain descriptive profile per actor type (medians / means of headline features).
actor_profile <- org_features %>% group_by(Actor_Type_label) %>%
  summarise(n = n(), followers_med = median(followers, na.rm = TRUE),
            cap_chars = median(cap_chars_med), pct_video = round(mean(pct_video), 2),
            onscreen  = round(mean(onscreen_text_rate), 2),
            eng_rate  = round(100 * median(eng_rate_med), 2), .groups = "drop") %>%
  arrange(desc(n))

actor_profile
# NGO/Foundation/Trust n = 12 (median 15.5k followers), Umbrella/Network n = 3
# (median 1.5k), then Conservancy, Consultancy, Government and Research/Academic as
# single orgs. NGO and Umbrella carry the field; the other four "types" are one
# organisation's style each.

write_csv(actor_profile, here("output/tables/org_actortype_profile.csv"))

# Follower counts are the only public bound on account-level audience size. One
# profile snapshot supplies one value per organization, so posts do not weight the
# median or the range.
audience_size <- org_features %>%
  summarise(n_organizations = sum(!is.na(followers)),
            median_followers = median(followers, na.rm = TRUE),
            rounded_median_followers = round(median(followers, na.rm = TRUE), -2),
            n_below_rounded_median = sum(followers < round(median(followers, na.rm = TRUE), -2),
                                         na.rm = TRUE),
            pct_below_rounded_median = round(100 * mean(followers < round(median(followers,
                                                                                 na.rm = TRUE), -2),
                                                        na.rm = TRUE), 1),
            minimum_followers = min(followers, na.rm = TRUE),
            maximum_followers = max(followers, na.rm = TRUE),
            smallest_account = Actor_Name[which.min(followers)],
            largest_account = Actor_Name[which.max(followers)])

write_csv(audience_size, here("output/tables/org_audience_size.csv"))
audience_size
# Across 19 organizations, the median is 4,109 followers. NACSO is smallest at
# 427 and Ocean Conservation Namibia largest at 236,580.


# ---- 2. Communication-style typology (clustering the orgs) -------------------
# The style fingerprint used to cluster orgs: caption length, hashtag / emoji use,
# link / question rates, visual devices, format mix, clip alignment, genre spread.
style_features <- c("cap_chars_med", "hashtags_med", "emojis_med", "link_rate",
                    "question_rate", "onscreen_text_rate", "protagonist_rate",
                    "data_visual_rate", "pct_video", "pct_carousel", "clip_align_med",
                    "genre_entropy")

# Cluster only orgs with enough posts for stable per-org feature estimates; the
# excluded low-volume orgs (2-3 posts = noisy profiles) are described separately.
min_posts        <- 10
cluster_orgs     <- org_features %>% filter(n_posts >= min_posts)                    # orgs kept for clustering
low_volume_orgs  <- org_features %>% filter(n_posts < min_posts) %>% pull(Actor_Name) # orgs set aside

nrow(cluster_orgs); low_volume_orgs
# 16 orgs clear the >= 10-post bar; 3 low-volume orgs are set aside (Tosco Trust,
# Youth4CAN, Desert Lion Conservation Trust).

# The Results text reports this count ("Three organizations posted fewer than 10
# times in the whole window"), so it is written out rather than left in a comment.
low_volume_tab <- org_features %>%
  filter(n_posts < min_posts) %>%
  dplyr::select(Actor_Name, n_posts) %>%
  arrange(n_posts)

write_csv(low_volume_tab, here("output/tables/org_low_volume.csv"))
low_volume_tab

# Build the feature matrix; fill any missing cell with that feature's median (so a
# single NA does not drop a whole org from the distance calculation).
feature_raw <- cluster_orgs %>% dplyr::select(all_of(style_features)) %>%
  mutate(across(everything(), ~ replace_na(., median(., na.rm = TRUE))))
# scale() z-standardises every feature (mean 0, SD 1) so no single large-unit
# feature (e.g. caption characters) dominates the Euclidean distance.
feature_z <- scale(feature_raw); rownames(feature_z) <- cluster_orgs$Actor_Name

# silhouette scan (k = 2..6). silhouette in [-1, 1] = how much better each org fits
# its own cluster than the nearest other cluster; a mean near 0 means no clean split.
silhouette_scan <- map_dfr(2:6, function(k) {
  fit <- kmeans(feature_z, centers = k, nstart = 50)          # 50 random starts, keep the best (avoids bad local optima)
  avg <- mean(silhouette(fit$cluster, dist(feature_z))[, 3])  # mean silhouette width at this k
  tibble(k = k, avg_sil = avg)
})

silhouette_scan %>% mutate(avg_sil = round(avg_sil, 3))
# Silhouette is uniformly low and rises only gently with k (k=2 .14, k=3 .17,
# k=4 .16, k=5 .17, k=6 .20): communication styles vary continuously, with no
# sharp cluster boundaries. Silhouette-max (k=6) would just isolate small outliers.

# Fix an interpretable k = 4 a priori (see the flat silhouette above) rather than
# chasing silhouette-max; report the modest silhouette honestly.
best_k <- 4
silhouette_scan$avg_sil[silhouette_scan$k == best_k]
# 0.164: weak, continuous structure -> treat the four groups as a descriptive
# typology, not as discrete, well-separated clusters.

# ---- The four-cluster partition itself [not reported] ------------------------
# Fixes the k = 4 solution and repeats it hierarchically, so the grouping behind
# the silhouette figure is on record.
kmeans_fit <- kmeans(feature_z, centers = best_k, nstart = 50)      # final k-means partition
ward_tree  <- hclust(dist(feature_z), method = "ward.D2")           # Ward hierarchical clustering (dendrogram view)

# Attach both cluster labels (k-means and the cut Ward tree) back to the org table.
org_clusters <- cluster_orgs %>% dplyr::select(Actor_Name, Actor_Type_label, n_posts, followers) %>%
  mutate(kmeans_cluster = kmeans_fit$cluster, hclust_cluster = cutree(ward_tree, best_k)) %>%
  bind_cols(cluster_orgs %>% dplyr::select(all_of(style_features)))

org_clusters %>% dplyr::select(Actor_Name, Actor_Type_label, kmeans_cluster, cap_chars_med,
                        pct_video, onscreen_text_rate, hashtags_med) %>% arrange(kmeans_cluster)
# Four groups of size 5 / 2 / 6 / 3. Cluster 1 = minimal-caption, no-hashtag style;
# cluster 2 = the video-led accounts (Ocean Conservation Namibia, AfriCat, ~82% video);
# cluster 3 = the text-on-image, higher-onscreen style; cluster 4 = long-caption,
# high-hashtag accounts.

# The silhouette scan behind the sentence "Average silhouette width stayed below
# .21 across k = 2 to 6 and reached .16 at the four-cluster solution" was printed
# to the console only, so that claim had no committed producer. It does now.
write_csv(silhouette_scan, here("output/tables/org_silhouette_scan.csv"))

# Cluster membership stays a CSV. The Results report the silhouette scan in prose
# rather than the membership itself, so no APA table is written for it.
write_csv(org_clusters, here("output/tables/org_clusters.csv"))

# Summarise each cluster: how many orgs, which orgs, and the typical style values.
org_cluster_profiles <- org_clusters %>% group_by(kmeans_cluster) %>%
  summarise(n = n(), orgs = paste(Actor_Name, collapse = "; "),
            cap = round(median(cap_chars_med)), video = round(mean(pct_video), 2),
            onscreen = round(mean(onscreen_text_rate), 2), hashtags = round(median(hashtags_med), 1),
            .groups = "drop")

org_cluster_profiles
# Cluster 1 (n=5): cap 399, 14% video, onscreen .06, 0 hashtags (spare captions).
# Cluster 2 (n=2): cap 558, 82% video, onscreen .14, 5 hashtags (video-led).
# Cluster 3 (n=6): cap 556, 11% video, onscreen .54, 0.5 hashtags (text-on-image).
# Cluster 4 (n=3): cap 626, 30% video, onscreen .18, 5 hashtags (long + hashtag-heavy).

write_csv(org_cluster_profiles, here("output/tables/org_cluster_profiles.csv"))


# ---- 3. Content-mix portfolio per org ----------------------------------------
# For each org, the share of its posts in each functional genre (a within-org
# proportion that sums to 1). This is the org's "content portfolio".
org_content_mix <- post_genres %>% count(Actor_Name, functional_genre) %>%
  group_by(Actor_Name) %>% mutate(share = n / sum(n)) %>% ungroup()

org_content_mix %>% arrange(Actor_Name, desc(share)) %>% head(10)
# Portfolios differ sharply by org: AfriCat is near-single-genre (research_dispatch
# 89%), while Cheetah Conservation Fund spreads across monitoring (34%), rescue (26%),
# org-update (17%) and a long tail. Most accounts lean on one or two dominant genres.

write_csv(org_content_mix, here("output/tables/org_content_mix.csv"))


# ---- Figures -----------------------------------------------------------------
# Actor-type effect sizes: bars of epsilon^2 with bootstrap 95% CIs (floored at 0).
high_volume_orgs <- org_features %>% filter(n_posts >= 15) %>% pull(Actor_Name)

# Machine genre keys are written out for the printed figure, because the thesis
# convention is that no snake_case variable name appears in a reported object. Title
# case, not sentence case: these strings are a figure LEGEND, and APA 7.27 says
# "Capitalize words in the legend using title case". Minor words stay lowercase per
# APA 6.17. The CSV at line 218 is written before this recode, so the machine keys
# in the data output are untouched.
genre_labels <- c(
  coexistence_community_livelihoods = "Coexistence and Community Livelihoods",
  commemoration_awareness           = "Commemoration and Awareness Days",
  ecotourism_aesthetic              = "Ecotourism and Landscape Aesthetics",
  education_youth                   = "Education and Youth",
  monitoring_conservation_tech      = "Monitoring and Conservation Technology",
  org_update_identity               = "Organizational Update and Identity",
  outlier_atypical                  = "Atypical or Unclassified",
  policy_governance                 = "Policy and Governance",
  rescue_intervention               = "Rescue and Intervention",
  research_dispatch                 = "Research Dispatch",
  species_spotlight_facts           = "Species Spotlight and Facts",
  weekly_ritual_filler              = "Weekly Greeting and Ritual",
  wildlife_series_broadcast         = "Wildlife Series and Broadcast")

fig_org_content_mix <- org_content_mix %>% filter(Actor_Name %in% high_volume_orgs) %>%
  mutate(functional_genre = dplyr::recode(functional_genre, !!!genre_labels),
         Actor_Name       = apa_org(Actor_Name)) %>%   # scraped profile strings -> prose spelling
  ggplot(aes(reorder(Actor_Name, share), share, fill = functional_genre)) +
  geom_col(width = .8) + coord_flip() +
  # Organisation names to the right of the bars. Under coord_flip the discrete axis
  # is still x, so position = "top" lands on the right. This puts the coloured bars
  # flush with the left edge of the figure, on the same line the legend starts on,
  # instead of indenting the whole panel behind a column of long names.
  scale_x_discrete(position = "top") +
  scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, .02))) +
  scale_fill_viridis_d(option = "viridis") +
  labs(x = NULL, y = "Share of Posts", fill = "Topic Category") +
  # Legend under the panel in three columns. A single right-hand column of 13 keys
  # took about a third of the canvas width and ran out after roughly half its
  # height, so the lower right of the figure was empty.
  guides(fill = guide_legend(ncol = 2, byrow = TRUE, title.position = "top",
                             keyheight = unit(9, "pt"))) +
  theme_apa(base_size = 11) +
  theme(legend.position = "bottom",
        # legend.location = "plot" measures the legend against the whole figure
        # rather than the panel. Without it the box is centred under the panel,
        # which the long organisation names push rightwards, and the second
        # column ran off the right edge (ggplot2 >= 3.5).
        legend.location = "plot", legend.justification = "left",
        legend.box.just = "left",
        # rel(.80) of base 11 is 8.8 pt, which stays at or above the 8 pt floor of
        # APA 7 section 7.26 after build.js scales the figure to 6.0 in.
        legend.text = element_text(size = rel(.80)),
        legend.key.size = unit(9, "pt"),
        # Default legend margin is 5.5 pt all round, which offset the legend from
        # the bars by about a tenth of an inch. Zeroed on the left so the coloured
        # keys and the bars begin on exactly the same line.
        legend.margin = margin(0, 0, 0, 0),
        # Left margin holds the "0%" tick label, which is centred on the panel edge
        # and would otherwise be cut in half now that the bars sit flush left.
        # The legend carries the same margin, so both still start on one line.
        plot.margin = margin(6, 8, 6, 14))

fig_org_content_mix
# Rendered at text width, because build.js places every figure at 6.0 in. A wider
# canvas prints scaled down. At 8 by 6 in it would print at 75% and its axis text
# would fall to about 7.6 pt, below the 8 pt floor of APA 7 section 7.26.
save_apa(fig_org_content_mix, "fig_org_content_mix", width = 6.5, height = 5.4)

# Posting cadence: posts per week over each org's active span (orgs with >= 10 posts).
# ---- Table B3: posting rhythm, calendar peaks and volume concentration -------
# Three quantities that describe WHEN and HOW MUCH the sector posts, rather than
# what it says. They come from the Python temporal features, which carry the
# per-month and per-event aggregates the R side does not recompute.
monthly <- read_csv(here("data/study1/features/temporal_monthly.csv"), show_col_types = FALSE)
events  <- read_csv(here("data/study1/features/temporal_events.csv"),  show_col_types = FALSE)

# Diagnostic only. The source retrieval is truncated for two of the three
# video-led accounts, so the monthly sequence cannot support a temporal trend.
# The output remains available for auditing the retrieval artefact but is not a
# reported Table B3 object.
rhythm_video <- monthly %>%
  transmute(domain = "Video share of monthly posts", entry = ym,
            all_orgs_pct        = round(100 * pct_video, 0),
            video_led_share_pct = round(100 * pct_posts_video_led, 0),
            other_16_orgs_pct   = round(100 * pct_video_rest, 0))

peaks <- events %>%
  arrange(desc(n_posts)) %>%
  transmute(domain = "Calendar peak", entry = nearest_event,
            date = event_date, n_posts, n_orgs)

# Monthly volume per organization. The collection window runs 1 November 2025 to
# 30 April 2026, six calendar months, so posts divided by six is the monthly rate.
WINDOW_MONTHS <- 6
volume <- org_cadence %>%
  transmute(Actor_Name, posts_per_month = round(n_posts / WINDOW_MONTHS, 1)) %>%
  arrange(desc(posts_per_month))
volume_summary <- tibble(
  domain = "Volume",
  entry  = c("Median monthly posts per organization", "Highest, Giraffe Conservation Foundation",
             "Lowest"),
  value  = c(round(median(volume$posts_per_month), 1),
             max(volume$posts_per_month), min(volume$posts_per_month))
)

# Posting rate across each account's RETRIEVED span. This avoids treating the
# missing early months of three truncated accounts as months with zero posts. It
# still describes the retrieved series, not a complete six-month history.
cadence_by_span <- org_cadence %>%
  transmute(Actor_Name, n_posts, first_to_last_span_days = span_days,
            active_days,
            posts_per_month_retrieved_span = round(n_posts / (span_days / (365.25 / 12)), 1),
            posts_per_month_nominal_window = round(n_posts / WINDOW_MONTHS, 1)) %>%
  arrange(desc(posts_per_month_retrieved_span))

cadence_summary <- cadence_by_span %>%
  summarise(n_organizations = n(),
            below_10_per_month_retrieved_span = sum(posts_per_month_retrieved_span < 10),
            pct_below_10_retrieved_span = round(100 * mean(posts_per_month_retrieved_span < 10), 1),
            median_per_month_retrieved_span = median(posts_per_month_retrieved_span),
            below_10_per_month_nominal_window = sum(posts_per_month_nominal_window < 10))

# Coverage audit on the retained post series. Sixteen accounts reach November
# 2025, while the three high-frequency series that begin in February or March
# 2026 are the ones known from the source-export audit to have hit the upstream
# per-profile ceiling. This output documents the dates used in the Method and
# keeps the coverage qualification separate from cadence.
retrieval_coverage <- raw_posts %>%
  group_by(Actor_Name, Instagram_Handle) %>%
  summarise(n_retrieved_posts = n(),
            first_retrieved_post = min(Posting_Date),
            last_retrieved_post = max(Posting_Date),
            .groups = "drop") %>%
  mutate(series_begins_february_or_later = first_retrieved_post >= as.Date("2026-02-01")) %>%
  arrange(first_retrieved_post)

retrieval_coverage_summary <- retrieval_coverage %>%
  summarise(n_organizations = n(),
            n_series_beginning_february_or_later = sum(series_begins_february_or_later),
            n_series_reaching_november = sum(first_retrieved_post < as.Date("2025-12-01")))

# The Ministry's 82 retrieved posts fell on 47 distinct active days within its
# 176-day retrieved span. Keeping that row in its own output makes the Results
# sentence traceable without reading the full cadence table.
ministry_cadence <- cadence_by_span %>%
  filter(Actor_Name == "Ministry of Environment Forestry & Tourism")

# Repeated named series are searched in the complete retrieved caption file. The
# patterns name the three formats established during the qualitative caption
# review. A format must appear at least twice to count as recurring.
series_rules <- tribble(
  ~series_format, ~series_pattern,
  "Thank Giraffe It's Friday", "thank\\s+giraffe\\s+it.?s\\s+friday|#thankgiraffeitsfriday",
  "Plant of the Month", "\\bplant\\s+of\\s+the\\s+month\\b",
  "Groen Namibie episodes", "\\bgroen\\s*:?\\s*namibi(?:ë|e)\\b|#groennamibi(?:ë|e)"
)

recurring_formats <- map2_dfr(series_rules$series_format, series_rules$series_pattern,
                              function(label, pattern) {
  raw_posts %>%
    filter(str_detect(Caption_Text, regex(pattern, ignore_case = TRUE))) %>%
    group_by(Actor_Name, Instagram_Handle) %>%
    summarise(series_format = label, n_posts = n(),
              first_post = min(Posting_Date), last_post = max(Posting_Date),
              .groups = "drop")
}) %>%
  filter(n_posts >= 2) %>%
  arrange(Actor_Name, series_format)

recurring_format_summary <- tibble(
  n_accounts = n_distinct(recurring_formats$Actor_Name),
  n_recurring_formats = nrow(recurring_formats),
  minimum_hits_per_format = 2
)

rhythm_video; peaks; volume_summary
# Video share rises from 14% of posts in November to 31% in April, but the split
# shows why: the three video-led accounts go from 4% to 31% of all monthly posts,
# while the other 16 organizations stay between 7% and 20%. The rise is a shift in
# who posts, not a sector-wide turn to video.
# The four largest calendar peaks are World Wildlife Day (29 posts, 13 orgs),
# Earth Day (27, 14), Earth Hour (25, 13) and World Pangolin Day (19, 11).
# Median volume is 8.8 posts per organization per month, with Giraffe Conservation
# Foundation highest at 13.8.
write_csv(rhythm_video,    here("output/tables/org_rhythm_video_share.csv"))
write_csv(peaks,           here("output/tables/org_calendar_peaks.csv"))
write_csv(volume,          here("output/tables/org_monthly_volume.csv"))
write_csv(volume_summary,  here("output/tables/org_volume_summary.csv"))
write_csv(cadence_by_span, here("output/tables/org_cadence_by_span.csv"))
write_csv(cadence_summary, here("output/tables/org_cadence_summary.csv"))
write_csv(retrieval_coverage, here("output/tables/org_retrieval_coverage.csv"))
write_csv(retrieval_coverage_summary,
          here("output/tables/org_retrieval_coverage_summary.csv"))
write_csv(ministry_cadence, here("output/tables/org_ministry_cadence.csv"))
write_csv(recurring_formats, here("output/tables/org_recurring_formats.csv"))
write_csv(recurring_format_summary, here("output/tables/org_recurring_format_summary.csv"))

cadence_summary; ministry_cadence; recurring_formats; recurring_format_summary
# Ten of 19 accounts average fewer than 10 posts per month over their retrieved
# span. Three accounts carry a repeated named format. The Ministry contributed 82
# posts on 47 active days within a 176-day retrieved span.
