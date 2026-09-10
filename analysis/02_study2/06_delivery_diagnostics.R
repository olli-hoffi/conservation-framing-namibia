# =============================================================================
# 02_study2/06_delivery_diagnostics.R
#
# WHERE THIS BELONGS
#   In the thesis: Chapter 4, Results: Delivery Diagnostics
#   Produces: Figure 10
#   Status: Exploratory. Study 2 has a preregistration, but this script is not
#   part of it. Only 04_confirmatory.R and 05_confirmatory_h2a.R are, and any
#   result here is a finding to be retested rather than a test.
#
# Study 2 - EXPLORATORY analyses on the richer Meta delivery data pulled by
# the Meta Ads toolkit, which is not part of this package. Everything here is
# NON-preregistered and reported as
# exploratory (descriptive or two-tailed, no alpha correction).
#
# Inputs (data/raw/MetaAds/, one CSV per round):
#   MetaAds_richmetrics_<r>_*.csv  video funnel p25..p100, avg watch, outbound,
#                                  delivery rankings, cost metrics
#   MetaAds_daily_<r>_*.csv        time_increment = 1 (ad x day)
#   MetaAds_region_<r>_*.csv       CTR by Namibia region
#   MetaAds_hourly_<r>_*.csv       CTR by hour of day
#   MetaAds_placement_<r>_*.csv    publisher_platform x position x device
#   data/processed/meta_ads_clean.csv  main clean per-ad file (3-s views, link clicks)
#
# Outputs (output/tables/):
#   study2_expl_video_funnel.csv       study2_expl_daily_ctr.csv
#   study2_expl_ctr_decay.csv          study2_expl_delivery_rankings.csv
#   study2_expl_region.csv             study2_expl_hourly.csv
#   study2_expl_device.csv
# Figures (output/figures/): fig_study2_daily_ctr
#
# Sections:
#   L. Video retention funnel by condition (+ APA figure)
#   M. Within-round CTR decay / daily time series (+ APA figure)
#   N. Delivery quality rankings by condition and round
#   O. Outbound-click CTR sensitivity
#   P. Delivery by Namibia region
#   Q. CTR by hour of day
#   R. Placement and device purity
#
# Run order: 01b -> 04 -> 04b -> 04c.
# =============================================================================

# pacman::p_load() installs a package if missing, then loads it, in one call.
pacman::p_load(here, tidyverse, broom, flextable, officer)

# Source every helper in src/ (theme_apa(), condition_colours, save_apa(),
# save_apa_table(), apa_width_full). walk() runs source() on each file for its
# side effect and returns nothing.
list.files(here("src"), full.names = TRUE) %>% walk(source)


# ---- Constants + helpers ----------------------------------------------------

# Neutral listed FIRST = the control/reference the motivational frames sit against.
cond_levels <- c("Neutral", "Empathy", "Social Norm", "Narrative")

# Recover the framing condition from an ad-name slug. The Meta breakdown CSVs carry
# ad_name but no clean condition column, so we pattern-match the slug and return an
# ordered factor (case_when picks the first matching rule).
cond_from_adname <- function(x) {
  factor(case_when(
    str_detect(x, "neutral")    ~ "Neutral",
    str_detect(x, "empathy")    ~ "Empathy",
    str_detect(x, "socialnorm") ~ "Social Norm",
    str_detect(x, "narrative")  ~ "Narrative"
  ), levels = cond_levels)
}

# Read every per-round CSV matching a filename pattern and row-bind them. Returns
# NULL (not an error) if none exist, so a missing breakdown skips cleanly.
read_round_glob <- function(pattern) {
  files <- list.files(here("data/raw/MetaAds"), pattern = pattern, full.names = TRUE)
  if (length(files) == 0) return(NULL)                     # nothing to read
  files %>% map_dfr(read_csv, show_col_types = FALSE)      # read each + stack rows
}


# ---- L. Video retention funnel by condition (EXPLORATORY) -------------------
# 3-second views come from the main clean file; p25..p100 from the rich-metrics
# pull. Retention is reported relative to the 3-s view (the point at which a viewer
# has actually stopped scrolling).
# DURATION CONFOUND: video length co-varies with condition (~38 s Neutral/Empathy,
# ~46 s Narrative, ~52 s Social Norm), so deeper-quartile retention is mechanically
# harder for the longer conditions. Descriptive only.

# Rich-metrics pull (per ad): quartile completions p25..p100, avg watch time, etc.
rich <- read_round_glob("^MetaAds_richmetrics_.*\\.csv$") %>%
  mutate(condition = cond_from_adname(ad_name))            # label each row's condition

# The 3-second view count lives in the MAIN clean file, so grab just those columns
# (transmute = mutate but keep only the named columns) to join onto the rich data.
meta_3s <- here("data/processed/meta_ads_clean.csv") %>%
  read_csv(show_col_types = FALSE) %>%
  transmute(round, ad_name, video_3s_views)

# Per-condition retention funnel: join in the 3-s views, then sum every stage across
# ads within a condition. avg_watch_s is impression-weighted; the p-quartiles are
# simple sums of completions.
funnel <- rich %>%
  left_join(meta_3s, by = c("round", "ad_name")) %>%
  group_by(condition) %>%
  summarise(
    avg_watch_s = round(weighted.mean(avg_watch_s, impressions), 2),  # mean seconds watched
    impressions = sum(impressions),
    v3s   = sum(video_3s_views),                     # denominator: viewers who stopped scrolling
    p25   = sum(video_p25), p50 = sum(video_p50),    # count who reached 25% / 50%
    p75   = sum(video_p75), p100 = sum(video_p100),  # count who reached 75% / 100%
    .groups = "drop"
  ) %>%
  mutate(
    r_p25  = round(p25 / v3s, 3),   # retained to 25% among 3-s viewers
    r_p50  = round(p50 / v3s, 3),   # retained to 50%
    r_p75  = round(p75 / v3s, 3),   # retained to 75%
    r_p100 = round(p100 / v3s, 3)   # retained to 100% (finished)
  )

funnel %>% dplyr::select(condition, v3s, r_p25, r_p50, r_p75, r_p100, avg_watch_s)
# Retention among 3-s viewers is similar across conditions and low overall: r_p25
# .36-.42, r_p100 .04-.07. Social Norm (the longest videos, ~52 s) retains least at
# every mark (r_p25 = .355, r_p100 = .041), consistent with the duration confound
# rather than a framing effect. avg watch 3.7-4.3 s -> most viewers drop within seconds.

write_csv(funnel, here("output/tables/study2_expl_video_funnel.csv"))
save_apa_table(funnel %>% dplyr::select(condition, v3s, r_p25, r_p50, r_p75, r_p100, avg_watch_s),
               "study2_expl_video_funnel",
               title = "Table. Video Retention Funnel by Framing Condition (Exploratory)",
               note  = "Retention shares are relative to 3-second viewers (v3s). Duration co-varies with condition.",
               digits = 3)

# APA figure: retention curve (share of 3-s viewers still watching at each mark).
# Convert each milestone count to a proportion of 3-s viewers (`3 s` becomes 1.0),
# then pivot wide -> long so each milestone is a row we can draw as a line point.
funnel_long <- funnel %>%
  dplyr::select(condition, `3 s` = v3s, `25%` = p25, `50%` = p50,
         `75%` = p75, `100%` = p100) %>%
  # across(): divide each milestone count by v3s (`3 s`); baseline milestone = 1.
  mutate(across(`25%`:`100%`, ~ .x / `3 s`), `3 s` = 1) %>%
  pivot_longer(-condition, names_to = "milestone", values_to = "retention") %>%  # one row per condition x milestone
  mutate(milestone = factor(milestone,                     # lock x-axis order
                            levels = c("3 s", "25%", "50%", "75%", "100%")))

# ---- M. Within-round CTR decay / daily time series (EXPLORATORY) ------------
# Does CTR fall within a round as the fixed audience is re-exposed (novelty decay),
# and is any decay uniform across conditions?

# Per-ad-per-day delivery (time_increment = 1). Parse the date string to a Date so
# it can be differenced into a day counter below.
daily <- read_round_glob("^MetaAds_daily_.*\\.csv$") %>%
  mutate(condition = cond_from_adname(ad_name),
         date = as.Date(date))

# Collapse to condition x day within each round, then add day_index = days since the
# round's first day (1, 2, 3, ...) so decay can be modelled as CTR ~ day_index.
daily_cond <- daily %>%
  group_by(round, condition, date) %>%
  summarise(impressions = sum(impressions),
            link_clicks = sum(link_clicks), .groups = "drop") %>%
  group_by(round) %>%
  mutate(day_index = as.integer(date - min(date)) + 1L) %>%   # first campaign day = 1
  ungroup() %>%
  mutate(ctr = link_clicks / impressions)

# Pooled daily CTR trend per round (log-odds of click ~ day_index). group_modify()
# runs the SAME model separately for each round; the binomial GLM regresses
# click-vs-no-click on day_index, so a NEGATIVE slope_logit = CTR falls each day
# (novelty / audience saturation).
decay <- daily_cond %>%
  group_by(round) %>%
  group_modify(~ {
    # Quasibinomial, for the same reason the primary CTR tests use it: clicks cluster
    # within advertisements, so a plain binomial understates the standard error. The
    # slope is unchanged by the switch, the interval is not. SE and CI are kept rather
    # than dropped, so the numbers reported in the text have a source in the package.
    m <- glm(cbind(link_clicks, impressions - link_clicks) ~ day_index,
             data = .x, family = quasibinomial())
    ci <- suppressMessages(confint.default(m)["day_index", ])
    tidy(m) %>% filter(term == "day_index") %>%              # keep only the day slope, drop intercept
      transmute(slope_logit = round(estimate, 3),           # daily change in log-odds of a click
                se       = round(std.error, 3),
                ci_low   = round(ci[1], 3),
                ci_high  = round(ci[2], 3),
                dispersion = round(summary(m)$dispersion, 2),
                p        = round(p.value, 4))
  }) %>% ungroup()

decay
# Round 1 shows a clear within-round CTR decay (slope_logit = -0.109, p < .001):
# click odds fall each day as the fixed audience is re-exposed. Round 2 is flat
# (slope = +0.018, p = .512) - no reliable decay. Novelty wear-out appears in R1 only.

write_csv(daily_cond, here("output/tables/study2_expl_daily_ctr.csv"))
write_csv(decay, here("output/tables/study2_expl_ctr_decay.csv"))

# APA figure: daily CTR per condition, faceted by round (ctr * 100 = percent).
p_daily <- daily_cond %>%
  ggplot(aes(day_index, ctr * 100, colour = condition, linetype = condition,
             group = condition)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.6) +
  facet_wrap(~ round, labeller = as_labeller(function(x) paste("Round", x))) +
  scale_colour_manual(values = condition_colours, name = NULL) +
  # APA 7.26: colour alone must not carry the distinction, so linetype doubles it.
  # Both scales share name = NULL, which merges them into a single legend.
  scale_linetype_manual(values = c("solid", "longdash", "dotted", "dotdash"),
                        name = NULL) +
  labs(title = "Daily Click-Through Rate Across Each Campaign Round",
       x = "Campaign Day", y = "CTR (%)") +
  theme_apa(legend_pos = "bottom")

p_daily
save_apa(p_daily, "fig_study2_daily_ctr", width = apa_width_full, height = 4)


# ---- N. Delivery quality rankings by condition and round (EXPLORATORY) ------
# Meta's relative rankings of each ad vs. competing ads for the same audience.

# Pull Meta's three relative rankings per ad (each = how this ad compared with other
# advertisers competing for the same audience: above/average/below average).
rankings <- rich %>%
  dplyr::select(round, condition, ad_name, quality_ranking,
         engagement_ranking, conversion_ranking) %>%
  arrange(round, condition)

rankings
# All 24 ads share near-identical Meta rankings, so condition does not separate on
# delivery quality: quality_ranking = BELOW_AVERAGE_35 for every R1 ad, engagement
# mostly AVERAGE, conversion mostly AVERAGE (R1) / BELOW_AVERAGE_35 (R2). The frames
# competed on an even delivery footing.

write_csv(rankings, here("output/tables/study2_expl_delivery_rankings.csv"))

# Tally how often each ranking value occurs, by round and ranking dimension (pivot
# the 3 ranking columns into rows, then count). Descriptive frequency table.
rank_summary <- rankings %>%
  pivot_longer(quality_ranking:conversion_ranking,
               names_to = "dimension", values_to = "rank") %>%
  count(round, dimension, rank) %>%     # frequency of each ranking bucket
  arrange(round, dimension, desc(n))    # most common first

rank_summary
# Confirms the uniformity: R1 quality is BELOW_AVERAGE_35 for all 12 ads, engagement
# AVERAGE for all 12, conversion AVERAGE for 10 (2 below); R2 quality splits 6/6
# AVERAGE vs. below, conversion BELOW_AVERAGE_35 for all 12. No condition stands out.


# ---- O. Outbound vs. inline link-click agreement [not reported] --------------
# Outbound clicks (left Instagram) vs. inline link clicks (the primary CTR
# numerator). If they coincide for every ad, CTR is robust to this choice.

outbound_chk <- rich %>%
  left_join(meta_3s %>% dplyr::select(round, ad_name), by = c("round", "ad_name")) %>%  # align keys
  left_join(here("data/processed/meta_ads_clean.csv") %>%
              read_csv(show_col_types = FALSE) %>%
              dplyr::select(round, ad_name, link_clicks_primary = link_clicks),         # bring in the primary count
            by = c("round", "ad_name")) %>%
  mutate(diff = outbound_clicks - link_clicks_primary)   # 0 = the two definitions agree

# n ads where outbound == inline, total ads, and the largest absolute discrepancy.
outbound_agreement <- tibble(
  n_agree    = sum(outbound_chk$diff == 0),
  n_ads      = nrow(outbound_chk),
  max_abs_diff = max(abs(outbound_chk$diff))
)

outbound_agreement
# They agree for 22 of 24 advertisements, largest difference 1 click.
# -> both definitions give the same picture, so the reported rate does not hang on
#    that choice. The inline count carries throughout.


# ---- P. Delivery and CTR by Namibia region (EXPLORATORY) --------------------
# Where in Namibia the ads were delivered, and CTR per region. imp_share = each
# region's fraction of total impressions; arrange by volume (biggest region first).
region <- read_round_glob("^MetaAds_region_.*\\.csv$")
region_summary <- region %>%
  group_by(region) %>%
  summarise(impressions = sum(impressions),
            link_clicks = sum(link_clicks), .groups = "drop") %>%
  mutate(imp_share = round(impressions / sum(impressions), 3),   # share of all impressions
         ctr = round(link_clicks / impressions, 4)) %>%          # region-level CTR
  arrange(desc(impressions))

region_summary
# Delivery is concentrated in Khomas (Windhoek): 53.7% of all impressions, then
# Erongo 13.5% and Oshana 8.3%. Region-level CTR is fairly flat (.0026-.0065), with
# the smaller northern regions (Kunene .0065, Ohangwena .0048) slightly higher but on
# thin volume. The audience skews strongly urban/central.

write_csv(region_summary, here("output/tables/study2_expl_region.csv"))


# ---- Q. CTR by hour of day [not reported] ------------------------------------
# Meta labels this "HH:00 - HH:59"; str_sub(..., 1, 2) takes the leading two digits
# as an integer hour (0-23) in the advertiser time zone.
hourly <- read_round_glob("^MetaAds_hourly_.*\\.csv$") %>%
  rename(hour_band = hourly_stats_aggregated_by_advertiser_time_zone) %>%  # shorten the long column name
  mutate(hour = as.integer(str_sub(hour_band, 1, 2)))

hourly_summary <- hourly %>%
  group_by(hour) %>%
  summarise(impressions = sum(impressions),
            link_clicks = sum(link_clicks), .groups = "drop") %>%
  mutate(ctr = round(link_clicks / impressions, 4))   # CTR for each hour of the day

hourly_summary %>% filter(ctr == max(ctr) | ctr == min(ctr))   # just the best + worst hour
# Highest at 07:00 (0.71%), lowest at 03:00 (0.31%).
# -> response roughly doubles from the small hours to the morning, a delivery
#    pattern rather than a framing effect. Delivery is reported by condition and
#    round, not by hour.

write_csv(hourly_summary, here("output/tables/study2_expl_hourly.csv"))


# ---- R. Placement and device purity (EXPLORATORY) ---------------------------
# Purity check on where the ads ran. distinct() lists every platform x position
# combination delivered - it should be Instagram Reels only, confirming the
# placement was not silently broadened by Meta.
placement <- read_round_glob("^MetaAds_placement_.*\\.csv$")

placement %>% distinct(publisher_platform, platform_position)
# Exactly one placement: instagram / instagram_reels. Meta did NOT silently broaden
# delivery beyond the intended Reels placement - the purity check passes.

# Which devices saw the ads (mobile vs. desktop etc.), as a share of impressions.
device_share <- placement %>%
  group_by(impression_device) %>%
  summarise(impressions = sum(impressions), .groups = "drop") %>%
  mutate(share = round(impressions / sum(impressions), 3)) %>%   # each device's impression share
  arrange(desc(impressions))

device_share
# Delivery is almost entirely mobile: Android smartphone 66.0% + iPhone 33.0% =
# ~99% of impressions; tablets and other devices are negligible (< 1%). A phone-first
# audience, as expected for Instagram Reels.

write_csv(device_share, here("output/tables/study2_expl_device.csv"))
