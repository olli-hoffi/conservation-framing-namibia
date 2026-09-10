# =============================================================================
# 02_study2/13_figures_remaining.R
#
# WHERE THIS BELONGS
#   In the thesis: Chapter 4, Results: Delivery Diagnostics and Exploratory Analyses
#   Produces: Figures 6 and F2
#   Status: Exploratory. Study 2 has a preregistration, but this script is not
#   part of it. Only 04_confirmatory.R and 05_confirmatory_h2a.R are, and any
#   result here is a finding to be retested rather than a test.
#
# Study 2 - the two APA-7 figures that visualise results otherwise living only in
# tables. Descriptive/diagnostic companions to the preregistered tests in
# 02_study2/04_confirmatory.R; neither is a confirmatory analysis.
#
# Figure type is matched to the data (APA 7 §7.30-7.36):
#   tile -> a value across a two-way category grid (heatmap)
#   dot  -> individual observations (the 3 creative variants per condition)
# Both figures use theme_apa(). The four framing conditions keep condition_colours,
# while the age-by-gender grid uses viridis so colour never implies a condition.
#
# Inputs:
#   data/processed/meta_ads_clean.csv      one row per ad = condition x variant x round
#   data/raw/MetaAds/MetaAds_demographics_*.csv   per-ad age x gender breakdown
# Outputs (output/figures/, 300-dpi PNG + PDF):
#   fig_study2_demographics_heatmap, fig_study2_variant_spread
# =============================================================================

# pacman::p_load() installs a package if missing, then loads it, in one call.
pacman::p_load(here, tidyverse, scales, ggrepel, flextable, officer)

# Source every helper in src/ (theme_apa(), condition_colours, save_apa(),
# apa_width_full/half, save_apa_table()). walk() runs source() on each file.
list.files(here("src"), full.names = TRUE) %>% walk(source)


# ---- Constants + small readers ----------------------------------------------

cond_levels <- c("Neutral", "Empathy", "Social Norm", "Narrative")   # fixed order

# Recover the framing condition from an ad-name slug (the breakdown CSVs carry no
# clean condition column). Returns an ordered factor so plots/facets sort right.
cond_from_adname <- function(x) {
  factor(case_when(
    str_detect(x, "neutral")    ~ "Neutral",
    str_detect(x, "empathy")    ~ "Empathy",
    str_detect(x, "socialnorm") ~ "Social Norm",
    str_detect(x, "narrative")  ~ "Narrative"
  ), levels = cond_levels)
}

# Read + row-bind every breakdown CSV matching a filename pattern (round 1 + 2).
read_glob <- function(pat) {
  here("data/raw/MetaAds") %>%
    list.files(pattern = pat, full.names = TRUE) %>%
    map_dfr(read_csv, show_col_types = FALSE)
}

# Main per-ad Meta export (one row per ad = condition x variant x round).
meta <- here("data/processed/meta_ads_clean.csv") %>%
  read_csv(show_col_types = FALSE) %>%
  mutate(condition = factor(condition, levels = cond_levels),
         round     = factor(round))


# ---- Figure: demographic CTR heatmap (age x gender, faceted by condition) -----
# Aggregate CTR within each age x gender cell per condition. Two main age bands +
# binary gender keeps the grid readable; str_to_title tidies the gender labels.
demo <- read_glob("^MetaAds_demographics_") %>%
  mutate(condition = cond_from_adname(ad_name)) %>%
  filter(gender %in% c("female", "male"), age %in% c("18-24", "25-34")) %>%
  group_by(condition, age, gender) %>%
  summarise(link_clicks = sum(link_clicks), impressions = sum(impressions),
            .groups = "drop") %>%
  mutate(ctr = link_clicks / impressions, gender = str_to_title(gender))

demo %>% arrange(desc(ctr)) %>% slice(1:3)
# CTR is highest for women 25-34 across every condition (Empathy .63%, Narrative
# .63%, Social Norm .56%) and lowest for men 18-24 (~.29-.35%). Age/gender moves CTR
# more than framing does - the age x gender gradient dominates the condition gradient.

# Heatmap: CTR is a continuous non-condition value -> viridis continuous fill.
# txt_col picks dark text on the light (high) cells and white on the dark (low)
# cells so every value stays legible against its fill (rescale over the plotted range).
demo <- demo %>%
  mutate(txt_col = if_else(scales::rescale(ctr) > 0.5, "grey10", "white"))

p_demo <- demo %>%
  ggplot(aes(age, gender, fill = ctr)) +
  geom_tile(colour = "white", linewidth = 1) +
  geom_text(aes(label = percent(ctr, accuracy = 0.01), colour = txt_col), size = 3) +
  facet_wrap(~ condition) +
  scale_fill_viridis_c(labels = percent_format(accuracy = 0.1), name = "CTR") +
  scale_colour_identity() +                                 # use txt_col literally
  labs(title = "Click-Through Rate by Age, Gender, and Framing Condition",
       x = "Age Band", y = NULL) +
  theme_apa() +
  theme(panel.grid = element_blank())   # belt-and-braces: no gridlines behind tiles

p_demo
save_apa(p_demo, "fig_study2_demographics_heatmap", width = apa_width_full, height = 4.5)


# ---- Figure: creative-variant CTR spread within condition (dot plot) ----------
# One CTR per variant (3 per condition), plus the condition mean as a reference bar.
# Dot plot is the APA figure type for showing individual observations.
variants <- meta %>%
  group_by(condition, variant) %>%
  summarise(ctr = sum(link_clicks) / sum(impressions), .groups = "drop") %>%
  # Derive a readable hook-variant label from the numeric variant id (1/2/3). The
  # figure's point is to compare the THREE creative hook variants, so colour maps
  # to the variant, not the framing condition.
  mutate(variant_lab = factor(paste0("Hook V", variant),
                              levels = c("Hook V1", "Hook V2", "Hook V3")))

cond_means <- variants %>%
  group_by(condition) %>%
  summarise(ctr = mean(ctr), .groups = "drop")   # unweighted mean of the 3 variant CTRs

cond_means %>% mutate(ctr = round(ctr * 100, 3))
# Condition-mean CTR: Narrative .49%, Empathy .43%, Social Norm .43%, Neutral .42%.
# Narrative's 3 variants (.44-.58%) sit above every other condition's spread; the
# other three overlap heavily - the same Narrative-on-top ordering H7 confirms.

# Dots = the 3 variants coloured by hook variant (viridis, 3-colour qualitative);
# geom_crossbar draws a flat mark at the condition mean. position_dodge keeps the
# three variant colours side by side within each condition so none overplot.
p_var <- variants %>%
  ggplot(aes(condition, ctr * 100)) +
  geom_crossbar(data = cond_means,
                aes(y = ctr * 100, ymin = ctr * 100, ymax = ctr * 100),
                width = 0.5, linewidth = 0.4, colour = "grey40") +
  geom_point(aes(colour = variant_lab, shape = variant_lab), size = 3, alpha = 0.9,
             position = position_dodge(width = 0.5)) +
  scale_colour_viridis_d(begin = 0.15, end = 0.8, name = "Hook Variant") +   # colour = variant
  # Shape repeats the variant so the three series do not rely on hue alone (APA 7.26).
  scale_shape_manual(values = c(16, 17, 15), name = "Hook Variant") +
  labs(title = "Click-Through Rate of the Three Creative Variants per Condition",
       x = NULL, y = "CTR (%)") +
  theme_apa()

p_var
save_apa(p_var, "fig_study2_variant_spread", width = apa_width_full, height = 4)
