# =============================================================================
# 02_study2/07_delivery_combined.R
#
# WHERE THIS BELONGS
#   In the thesis: Chapter 4, Results: Exploratory Analyses
#   Produces: Figure 7
#   Status: Exploratory. Study 2 has a preregistration, but this script is not
#   part of it. Only 04_confirmatory.R and 05_confirmatory_h2a.R are, and any
#   result here is a finding to be retested rather than a test.
#
# Combined-metrics figure: all Study 2 delivery, click, video, engagement and
# cost metrics in four panels, one per unit family. Grouped bars by condition.
#
# Inputs:
#   data/processed/meta_ads_clean.csv
#   data/raw/MetaAds/MetaAds_richmetrics_*.csv
# Output:
#   output/figures/fig_study2_combined_metrics.png (.pdf)
# =============================================================================

pacman::p_load(here, tidyverse, scales, patchwork)

list.files(here("src"), full.names = TRUE) %>% walk(source)

cond_levels <- c("Neutral", "Empathy", "Social Norm", "Narrative")

angled_x <- theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))

# ---- Read data ---------------------------------------------------------------

meta <- here("data/processed/meta_ads_clean.csv") %>%
  read_csv(show_col_types = FALSE) %>%
  mutate(condition = factor(condition, levels = cond_levels))

rich <- here("data/raw/MetaAds") %>%
  list.files(pattern = "richmetrics", full.names = TRUE) %>%
  map_dfr(read_csv, show_col_types = FALSE) %>%
  mutate(condition = factor(case_when(
    str_detect(ad_name, "neutral")    ~ "Neutral",
    str_detect(ad_name, "empathy")    ~ "Empathy",
    str_detect(ad_name, "socialnorm") ~ "Social Norm",
    str_detect(ad_name, "narrative")  ~ "Narrative"
  ), levels = cond_levels)) %>%
  # spend lives only in the processed campaign file, so it is joined on ad and round.
  # link_clicks is taken from that file too, not from the rich export. The two
  # sources disagree on one ad (WC_empathy_3, round 1: 91 in the rich export against
  # 90 in the processed file, totals 1,888 against 1,887). Every other analysis and
  # the chapter use the processed file, so the cost panel follows it as well, which
  # is what keeps this figure and the cost sentence in the text on the same number.
  left_join(read_csv(here("data/processed/meta_ads_clean.csv"), show_col_types = FALSE) %>%
              dplyr::select(ad_name, round, spend, link_clicks_clean = link_clicks),
            by = c("ad_name", "round"))

# ---- Panel 1: Click rates (% of impressions) --------------------------------

click_rates <- meta %>%
  group_by(condition) %>%
  summarise(
    imp = sum(impressions),
    `Link CTR`       = sum(link_clicks) / imp * 100,
    `All-Click Rate` = sum(clicks) / imp * 100,
    .groups = "drop"
  ) %>%
  left_join(
    rich %>%
      group_by(condition) %>%
      summarise(
        imp = sum(impressions),
        `Outbound CTR` = sum(outbound_clicks) / imp * 100,
        .groups = "drop"
      ) %>% dplyr::select(-imp),
    by = "condition"
  ) %>%
  dplyr::select(-imp) %>%
  pivot_longer(-condition, names_to = "metric", values_to = "rate") %>%
  mutate(metric = factor(metric, levels = c("Link CTR", "Outbound CTR", "All-Click Rate")))

p1 <- click_rates %>%
  ggplot(aes(metric, rate, fill = condition)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_manual(values = condition_colours) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(x = NULL, y = "% of Impressions", fill = NULL,
       title = "A. Click Rates") +
  theme_apa(legend_pos = "none") +
  angled_x

# ---- Panel 2: Video retention (% of 3-second viewers) -----------------------

# Relative progress only, so every bar in this panel means the same thing in
# every condition. ThruPlay cannot join them. It is Meta's billing unit and counts
# an absolute threshold of fifteen seconds, which lands at a different share of
# the video in each condition because duration co-varies with condition: 39.0% of
# the empathy advertisements, 38.7% of the neutral, 32.3% of the narrative and
# 29.0% of the social-norm ones. Beside the quartile marks its flat rates would
# read as equal retention, when in fact the social-norm audience clears the bar
# after 29% of its video and the empathy audience only after 39%. ThruPlay
# therefore appears in Panel D, where it serves as a cost denominator.
v3s_by_cond <- meta %>%
  group_by(condition) %>%
  summarise(v3s = sum(video_3s_views), imp = sum(impressions), .groups = "drop")

video_all <- rich %>%
  group_by(condition) %>%
  summarise(across(c(thruplays, video_p25, video_p50, video_p75, video_p100), sum),
            .groups = "drop") %>%
  left_join(v3s_by_cond, by = "condition") %>%
  transmute(condition,
            `25%`  = video_p25  / v3s * 100,
            `50%`  = video_p50  / v3s * 100,
            `75%`  = video_p75  / v3s * 100,
            `100%` = video_p100 / v3s * 100) %>%
  pivot_longer(-condition, names_to = "metric", values_to = "rate") %>%
  mutate(metric = factor(metric,
    levels = c("25%", "50%", "75%", "100%")))

# The quartile marks keep Meta's own notation, which is also what the variables
# are named after. Bare percentages under a percentage y axis could be misread as
# "a quarter of something", so the quantity they belong to is named in the figure
# note. It cannot go on an x axis title, which would be the only one in the figure
# and would sit orphaned in the row gap, nor in the panel title, which is too
# narrow to hold it.

# Entry is not drawn. On this denominator it is 100% by construction, and as a
# share of impressions it moves by 0.6 percentage points across conditions, so
# it is one number for the note rather than four near-identical bars.
entry_rate <- v3s_by_cond %>% transmute(condition, pct = round(v3s / imp * 100, 1))
tp_rate <- rich %>% group_by(condition) %>% summarise(tp = sum(thruplays), .groups = "drop") %>%
  left_join(v3s_by_cond, by = "condition") %>% transmute(condition, pct = round(tp / v3s * 100, 1))
message("Entry, 3-s views as % of impressions:"); print(entry_rate)
message("ThruPlay as % of 3-s viewers (reported in the note, not drawn):"); print(tp_rate)
# Entry into the video is flat across conditions, 18.6% to 19.2% of impressions
# reaching three seconds, a spread of 0.6 percentage points. Framing therefore did
# not move who started watching. ThruPlay reaches 30.5% to 32.0% of those viewers,
# but see the caveat above, that threshold means a different share of the video in
# each condition and the numbers are not comparable as retention.

p2 <- video_all %>%
  ggplot(aes(metric, rate, fill = condition)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_manual(values = condition_colours) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(x = NULL, y = "% of 3-Second Viewers", fill = NULL,
       title = "B. Video Retention") +
  theme_apa(legend_pos = "none") +
  angled_x

# ---- Panel 3: Engagement (per 1,000 impressions) ----------------------------

engagement <- meta %>%
  group_by(condition) %>%
  summarise(
    imp = sum(impressions),
    Reactions = sum(reactions) / imp * 1000,
    Shares    = sum(shares) / imp * 1000,
    Saves     = sum(saves) / imp * 1000,
    Comments  = sum(comments) / imp * 1000,
    .groups   = "drop"
  ) %>%
  dplyr::select(-imp) %>%
  pivot_longer(-condition, names_to = "metric", values_to = "rate") %>%
  mutate(metric = factor(metric,
    levels = c("Reactions", "Shares", "Saves", "Comments")))

# Points, not bars, because the axis is logarithmic. A bar encodes its value as a
# length from a zero baseline, and zero is unreachable on a log axis, so bars here
# would start at 1.00 and every value below 1 would hang downward at a length that
# does not correspond to its value.
#
# The four dodged points sit in the same left-to-right order as the bars in the
# other panels, so position identifies the conditions and the collected legend at
# the foot covers this panel too. Shape is mapped in addition to fill so the points
# stay separable where they overlap and do not rely on hue alone (APA 7.26). Both
# guides are suppressed here: a second point guide cannot merge with the bar guides
# and plot_layout(guides = "collect") would set it beside them, which overruns the
# figure width. The same four colours mean the same four conditions as in the
# three bar panels.
p3 <- engagement %>%
  ggplot(aes(metric, rate, fill = condition)) +
  geom_point(aes(shape = condition), position = position_dodge(width = 0.6),
             size = 2.4, stroke = 0) +
  scale_fill_manual(values = condition_colours) +
  # Shape carries the condition alongside colour, so the four series stay
  # distinguishable in greyscale and for colour-blind readers (APA 7.26).
  scale_shape_manual(values = c(21, 22, 23, 24)) +
  scale_y_log10(labels = label_number(accuracy = 0.01)) +
  guides(fill = "none", shape = "none") +
  labs(x = NULL, y = "Per 1,000 Impressions (Log)", fill = NULL,
       title = "C. Engagement") +
  theme_apa(legend_pos = "none") +
  angled_x

# ---- Panel 4: Cost (EUR) ----------------------------------------------------

cost <- rich %>%
  group_by(condition) %>%
  summarise(
    # Total spend over total outcome, which is what "cost per link click" means at
    # condition level. Weighting each ad's own ratio by IMPRESSIONS (the previous
    # form) only equals that when the click rate is constant across ads, and it is
    # not. spend is joined from meta_ads_clean because the rich-metrics export
    # carries no spend column, and reconstructing it as cost_per_link_click *
    # link_clicks loses a rounding digit that moves the third decimal. This now
    # matches sum(spend)/sum(link_clicks) as used in 02_study2/14_exploratory.R.
    `Cost / Link Click` = sum(spend, na.rm = TRUE) / sum(link_clicks_clean, na.rm = TRUE),
    `Cost / ThruPlay`   = sum(spend, na.rm = TRUE) / sum(thruplays, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(-condition, names_to = "metric", values_to = "eur") %>%
  # Short facet labels. The panel title already carries "Cost", so repeating it in
  # each strip only costs width the narrow panel does not have.
  mutate(metric = factor(metric,
    levels = c("Cost / Link Click", "Cost / ThruPlay"),
    labels = c("Per Link Click", "Per ThruPlay")))

p4 <- cost %>%
  ggplot(aes(condition, eur, fill = condition)) +
  geom_col(width = 0.7) +
  facet_wrap(~ metric, scales = "free_y", nrow = 1) +
  scale_fill_manual(values = condition_colours) +
  # Cost per link click (~EUR 0.12) and cost per ThruPlay (~EUR 0.009) differ by
  # a factor of about 13, so on one shared linear axis the ThruPlay bars collapse
  # to slivers and their between-condition differences vanish. A log axis is not
  # the fix here, because bars are drawn from zero and zero has no place on a log
  # scale. Each cost measure therefore gets its own linear panel. Conditions are
  # identified by fill and by the collected legend at the foot, so the x labels
  # inside the facets would only repeat it.
  scale_y_continuous(labels = function(x) formatC(x, format = "f", digits = 3),
                     expand = expansion(mult = c(0, 0.08))) +
  labs(x = NULL, y = "EUR", fill = NULL,
       title = "D. Cost") +
  theme_apa(legend_pos = "none") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        strip.text = element_text(size = 8.5),
        panel.spacing.x = unit(0.8, "lines"))

# ---- Combine panels ----------------------------------------------------------

p_combined <- (p1 | p2) / (p3 | p4) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        legend.text = element_text(size = 9),
        plot.title = element_text(size = 11, face = "bold", hjust = 0))

save_apa(p_combined, "fig_study2_combined_metrics",
         width = apa_width_full, height = 7)

message("Done: fig_study2_combined_metrics")
