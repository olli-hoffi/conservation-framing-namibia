# =============================================================================
# 02_study2/14_exploratory.R
#
# WHERE THIS BELONGS
#   In the thesis: Chapter 4, Results: Moderator Analyses and Exploratory Analyses
#   Produces: Tables 11 and F1, the engagement rows of Table 16 (its CTR column
#   comes from 09_engagement_models.R), plus the descriptive H6-related
#   coefficients reported in the Results
#   Status: Exploratory. Study 2 has a preregistration, but this script is not
#   part of it. Only 04_confirmatory.R and 05_confirmatory_h2a.R are, and any
#   result here is a finding to be retested rather than a test.
#
# Study 2 - EXPLORATORY analyses (NOT preregistered).
#
# EVERYTHING in this script is EXPLORATORY: it is either not preregistered at
# all, or explicitly DEVIATES from a preregistered gate. It must be reported as
# exploratory in the thesis (Ch 4, exploratory subsection); JARS requires a
# clear separation of confirmatory from exploratory results. No causal claim:
# Study 2 is a naturalistic comparison (Meta's algorithm chose who saw what).
#
# Contents:
#   A. Exit-survey descriptives (5 moderator items + demographics)
#   B. H2a, H6 + collectivism computed DESPITE the preregistered exit-survey N >= 20 gate
#      (deviation; run on explicit request; ad-level CTR join)
#   C. State manipulation items by condition (sympathy, perspective-taking)
#   D. H5 sensitivity: cells that GA4 attributes to Namibia
#   E. Meta engagement beyond CTR: video completion, reactions, cost metrics
#   F. CTR decline across rounds: condition x round interaction
#   G. Clicks vs. dwell: ad-level CTR vs. GA4 session duration (n = 12 ads)
#   H. Hook analysis: the first 3 seconds (hook rate, hold rate)
#   I. Variant heterogeneity within conditions
#   J. GA4 on-page engagement by condition (all four)
#   K. Delivery demographics (age x gender per condition)
#   L. H2a via Meta demographics (gender x empathy framing)
#
# Effect sizes carry bootstrap 95% CIs (percentile, 5000 resamples). Bootstrapping
# quantifies uncertainty WITHOUT normality assumptions - it adds no power at these
# sample sizes.
#
# Inputs  (data/processed/, from 02_study2/03_process_campaign.R):
#   meta_ads_clean.csv    one row per ad = condition x variant x round
#   ga4_clean.csv         landing-page sessions per UTM
#   exitsurvey_clean.csv  one row per exit-survey respondent
# Optional (data/raw/MetaAds/): MetaAds_demographics_*.csv (Sections K + L)
#
# Run order: 02_study2/03_process_campaign.R -> 02_study2/04_confirmatory.R -> this script.
#
# out: output/tables/study2_expl_demographics_gender.csv      (Table 11, + .docx)
#      output/tables/study2_expl_demographics_age.csv         (Table 11, + .docx)
#      output/tables/study2_expl_moderators.csv               (exploratory moderator output, + .docx)
#      output/tables/study2_expl_exitsurvey_descriptives.csv  (by condition, + .docx)
#      output/tables/study2_expl_exitsurvey_pooled.csv        (Table F1)
#      output/tables/study2_expl_engagement.csv               (Table 16, engagement rows)
#      output/tables/study2_expl_ga4_engagement.csv           (Table 16, dwell rows)
#      output/tables/study2_expl_ga4_mean_ci.csv              (dwell intervals)
#      output/tables/study2_expl_hooks.csv                    (hook rates)
#      output/tables/study2_expl_hooks_adlevel_rates.csv      (per advertisement)
#      output/tables/study2_expl_hooks_adlevel_corr.csv       (the correlation Figure F1 shows)
#      output/tables/study2_expl_hook_glm.csv                 (hook model)
#      output/tables/study2_expl_completion_glm.csv           (completion model)
#      output/tables/study2_expl_ctr_vs_dwell.csv             (clicks vs dwell, + .docx)
#      output/tables/study2_expl_ctr_vs_dwell_corr.csv        (its coefficient)
#      output/tables/study2_expl_ctr_round_interaction.csv    (round interaction)
#      output/tables/study2_expl_variant_heterogeneity.csv    (within condition)
#      output/tables/study2_expl_h2a_gender_contrast.csv      (H2a via Meta)
#      output/tables/study2_expl_h5_namibia.csv               (H5 sensitivity)
#      output/tables/study2_expl_h6_item_redundancy.csv       (H6 items)
#      output/tables/study2_expl_demographics_by_round.csv    (delivery by round)
# =============================================================================

# pacman::p_load() installs a package if missing, then loads it, in one call.
pacman::p_load(here, tidyverse, broom, effectsize, flextable, officer)

# Source every helper in src/ (theme_apa(), condition_colours, save_apa(),
# save_apa_table(), fmt_* formatters). walk() runs source() on each file for its
# side effect and returns nothing.
list.files(here("src"), full.names = TRUE) %>% walk(source)


# ---- Analysis-wide constants ------------------------------------------------

set.seed(42)     # lock the bootstrap resamples so every CI reproduces exactly
n_boot <- 5000   # bootstrap resamples for every percentile CI in this script

# Neutral listed FIRST so it becomes the factor reference level - the control the
# motivational frames are compared against.
cond_levels  <- c("Neutral", "Empathy", "Social Norm", "Narrative")
motivational <- c("Empathy", "Social Norm", "Narrative")   # everything that is NOT the control


# ---- Load data --------------------------------------------------------------

# Cleaned per-ad Meta export: one row per ad = condition x variant x round.
meta <- here("data/processed/meta_ads_clean.csv") %>%
  read_csv(show_col_types = FALSE) %>%
  mutate(
    condition = factor(condition, levels = cond_levels),   # fix condition order
    round     = factor(round),                             # a grouping label, not a quantity
    ad_ctr    = link_clicks / impressions                  # per-ad CTR = the confirmatory DV
  )

# GA4 = Google Analytics 4: the landing-page side (sessions, dwell time).
ga4 <- here("data/processed/ga4_clean.csv") %>%
  read_csv(show_col_types = FALSE) %>%
  mutate(condition = factor(condition, levels = cond_levels))

# Exit survey: the voluntary post-click questionnaire (5 moderator items).
exitsurvey <- here("data/processed/exitsurvey_clean.csv") %>%
  read_csv(show_col_types = FALSE) %>%
  mutate(
    condition = factor(condition, levels = cond_levels),
    round     = factor(round),
    is_female = if_else(gender == 1, 1L, 0L)   # DM02: 1 = Woman -> 1, everyone else -> 0
  )

nrow(meta); nrow(ga4); nrow(exitsurvey)
# 24 Meta ad rows (12 ads x 2 rounds), 38 GA4 UTM rows, 13 exit-survey responses.


# ---- Spearman rho with percentile bootstrap CI ------------------------------
# Spearman (rank correlation, not Pearson) because CTR / survey items are small-N,
# ordinal or skewed: it correlates ranks and assumes no normality/linearity.
spearman_boot <- function(x, y, reps = n_boot) {   # reps defaults to the global n_boot
  ok <- complete.cases(x, y)   # keep only pairs where BOTH x and y are present
  x  <- x[ok]; y <- y[ok]      # drop the incomplete pairs from both vectors
  n  <- length(x)              # effective sample size after listwise deletion

  # Guard: a correlation is undefined with < 3 points or if either side is constant
  # (zero variance -> no ranks to correlate). Return an all-NA row instead of erroring.
  if (n < 3 || sd(x) == 0 || sd(y) == 0) {
    return(tibble(n = n, rho = NA_real_, ci_low = NA_real_,
                  ci_high = NA_real_, p = NA_real_,
                  note = "not estimable (n < 3 or zero variance)"))
  }

  rho <- cor(x, y, method = "spearman")   # point estimate (rank correlation)
  # Asymptotic p-value; exact = FALSE avoids the exact algorithm (it fails with tied
  # ranks); suppressWarnings hides the routine "cannot compute exact p" note.
  p   <- suppressWarnings(cor.test(x, y, method = "spearman", exact = FALSE)$p.value)

  # Percentile bootstrap: resample the pairs WITH replacement, recompute rho each
  # time; the spread of those rho's stands in for the sampling distribution.
  boot_r <- replicate(reps, {
    i <- sample(n, replace = TRUE)                          # random indices, with replacement
    suppressWarnings(cor(x[i], y[i], method = "spearman"))  # rho on this resample
  })
  ci <- quantile(boot_r, c(0.025, 0.975), na.rm = TRUE)     # 2.5th / 97.5th pct = 95% CI

  tibble(n = n, rho = round(rho, 3),
         ci_low = round(ci[1], 3), ci_high = round(ci[2], 3),
         p = round(p, 4), note = "")
}


# ---- A. Exit-survey descriptives --------------------------------------------
# One summary row per condition: who answered the exit survey and how they scored
# on the five moderator items. Pure description (no test); N is tiny (13 total).

expl_exit_desc <- exitsurvey %>%
  group_by(condition) %>%
  summarise(
    n           = n(),                                   # respondents in this condition
    n_female    = sum(is_female, na.rm = TRUE),          # how many identified as women
    age_median  = median(age_group, na.rm = TRUE),       # 1 = 18-24, 2 = 25-29, 3 = 30-35
    emp_state_m = round(mean(emp_state, na.rm = TRUE), 2),    # mean state empathy (item 1)
    ident_m     = round(mean(ident_state, na.rm = TRUE), 2),  # mean perspective-taking (item 2)
    env_conc_m  = round(mean(env_concern, na.rm = TRUE), 2),  # mean environmental concern (item 3)
    bio_water_m = round(mean(bio_water, na.rm = TRUE), 2),    # mean biospheric water value (item 4)
    collect_m   = round(mean(collectivism, na.rm = TRUE), 2), # mean collectivism (item 5)
    .groups = "drop"
  )

# ---- Exit-survey descriptives by condition [not reported, partial] -----------
expl_exit_desc
# Respondents cluster in narrative (n = 6) and social norm (n = 4), empathy n = 2,
# neutral n = 1.
# -> the counts carry into the text, the five per-condition means do not. Those
#    items are reported pooled across all 13 respondents in Table F1, because
#    single cells fall to n = 1.

write_csv(expl_exit_desc, here("output/tables/study2_expl_exitsurvey_descriptives.csv"))
save_apa_table(expl_exit_desc, "study2_expl_exitsurvey_descriptives",
               title = "Table. Exit-Survey Descriptives by Condition (Exploratory)",
               note  = "N = 13. Descriptive only; cells too small for inference.",
               digits = 2)


# Table F1 reports the five moderator items pooled over all respondents, not split
# by condition, because the per-condition cells run down to n = 1. Mean, standard
# deviation, median and observed range are written out here so the appendix table
# has a file behind it rather than a console reading. No resampling happens in this
# block, so it consumes nothing from the bootstrap stream opened at the top.
exit_items <- c(
  "State empathic concern"         = "emp_state",
  "State character identification" = "ident_state",
  "General environmental concern"  = "env_concern",
  "Biospheric value (water)"       = "bio_water",
  "Horizontal collectivism"        = "collectivism"
)

expl_exit_pooled <- imap_dfr(exit_items, function(col, label) {
  v <- exitsurvey[[col]]
  v <- v[!is.na(v)]
  tibble(
    item   = label,
    n      = length(v),
    m      = round(mean(v), 2),
    sd     = round(sd(v), 2),
    mdn    = round(median(v), 1),
    v_min  = min(v),
    v_max  = max(v)
  )
})

expl_exit_pooled
# State empathic concern is the highest and least dispersed of the five items, and
# biospheric value and horizontal collectivism carry identical entries because every
# respondent answered those two items the same way.

write_csv(expl_exit_pooled, here("output/tables/study2_expl_exitsurvey_pooled.csv"))


# ---- B. H2a, H6 + collectivism DESPITE the N >= 20 gate ---------------------
# PREREGISTRATION DEVIATION: the prereg gates these tests at exit-survey N >= 20;
# observed N = 13. Computed anyway on explicit request - report as EXPLORATORY
# with the deviation stated, never as a confirmatory test. Each respondent is
# joined to the CTR of the SPECIFIC ad (round x condition x variant) that referred
# them, so CTR varies within a condition. Caveat: CTR is an ad-level property
# (n = 12 ads), not an individual behaviour of the respondent.

# left_join keeps every survey row and pulls in the referring ad's CTR.
tf <- exitsurvey %>%
  left_join(meta %>% dplyr::select(round, condition, variant, ad_ctr),
            by = c("round", "condition", "variant"))

# Run the same Spearman-with-bootstrap once per moderator hypothesis, tag each
# result, then stack the rows. One hypothesis = one test.
mods <- bind_rows(
  # H2a: female (vs. other) x CTR among Empathy respondents
  spearman_boot(filter(tf, condition == "Empathy")$is_female,
                filter(tf, condition == "Empathy")$ad_ctr) %>%
    mutate(hypothesis = "H2a", moderator = "female", subset = "Empathy"),
  # H6.1: environmental concern x CTR, pooled across the 3 motivational conditions
  spearman_boot(filter(tf, condition %in% motivational)$env_concern,
                filter(tf, condition %in% motivational)$ad_ctr) %>%
    mutate(hypothesis = "H6.1", moderator = "env_concern", subset = "pooled motivational"),
  # H6.2: biospheric value (water) x CTR, pooled across motivational conditions
  spearman_boot(filter(tf, condition %in% motivational)$bio_water,
                filter(tf, condition %in% motivational)$ad_ctr) %>%
    mutate(hypothesis = "H6.2", moderator = "bio_water", subset = "pooled motivational")
) %>%
  dplyr::select(hypothesis, moderator, subset, n, rho, ci_low, ci_high, p, note) %>%   # tidy column order
  mutate(status = "EXPLORATORY (prereg N>=20 gate not met)")   # stamp the deviation on every row

mods
# All three moderators run on n = 2-12 pairs. H2a not estimable (n = 2).
# H6.1 rho = .254 (n = 12, p = .426), H6.2
# rho = .387 (n = 12, p = .214). Every CI spans zero and no p approaches .05 -
# none is interpretable at these n's. Reported as noise.

write_csv(mods, here("output/tables/study2_expl_moderators.csv"))
save_apa_table(mods, "study2_expl_moderators",
               title = "Table. Exploratory Exit-Survey Moderator Estimates for H2a and H6",
               note  = paste("Spearman rho with percentile-bootstrap 95% CIs.",
                             "The preregistered exit-survey gate was N >= 20 (observed = 13).",
                             "The H6 participant rows map to eight advertisement-rounds, so participants",
                             "referred by the same advertisement share its CTR and do not provide independent outcomes."),
               digits = 3)


# ---- B2. Redundancy inspection for the two H6 items -------------------------
# The Method promises that Items 3 (general environmental concern) and 4 (biospheric
# water value) were inspected for redundancy before they entered H6, with a composite
# to be used should they prove redundant. The Results state the outcome, that both were
# retained as separate correlates, but the inspection's own number was never reported,
# so a reader could not check the decision. Computed here on the same pooled
# motivational set the H6 correlations use, with the same estimator.
# The bootstrap draws from the single stream opened by set.seed(42) at the top of this
# script. Adding a resampling step in the middle would consume draws and silently shift
# every CI computed after it, so the stream is saved here and restored below. Verified:
# without the restore, this block consumes draws and shifts the hook-rate CI away from
# the published [.42, .98], the interval behind Figure F1.
.rng_before_redundancy <- .Random.seed

# One bootstrap, one set of numbers. Calling spearman_boot() as well would draw a second
# time from the same stream and produce a second, different interval in the same row.
h6_items <- filter(tf, condition %in% motivational)
h6_ok    <- complete.cases(h6_items$env_concern, h6_items$bio_water)
h6_x     <- h6_items$env_concern[h6_ok]
h6_y     <- h6_items$bio_water[h6_ok]

h6_rho <- cor(h6_x, h6_y, method = "spearman")
h6_p   <- suppressWarnings(cor.test(h6_x, h6_y, method = "spearman", exact = FALSE)$p.value)
h6_boot <- replicate(n_boot, {
  i <- sample(length(h6_x), replace = TRUE)
  suppressWarnings(cor(h6_x[i], h6_y[i], method = "spearman"))
})
# Four decimals, because three leaves a bound like -0.355 ambiguous at the two decimals
# the thesis reports.
h6_ci <- round(quantile(h6_boot, c(0.025, 0.975), na.rm = TRUE, names = FALSE), 4)

h6_redundancy <- tibble(
  n       = length(h6_x),
  rho     = round(h6_rho, 3),
  ci_low  = h6_ci[1],
  ci_high = h6_ci[2],
  p       = round(h6_p, 4),
  pair    = "env_concern x bio_water (H6 items 3 and 4)",
  status  = "EXPLORATORY (redundancy inspection promised in the Method)"
)

h6_redundancy
# Near zero. The two items are not redundant, which is why they stayed separate.

write_csv(h6_redundancy, here("output/tables/study2_expl_h6_item_redundancy.csv"))

.Random.seed <- .rng_before_redundancy   # hand the stream back untouched


# ---- C. State items by condition (manipulation-consistency check) -----------
# If the framing worked as intended, Empathy viewers should score highest on state
# sympathy and Narrative viewers highest on perspective-taking. Purely descriptive
# at these n's.

expl_state <- exitsurvey %>%
  group_by(condition) %>%
  summarise(
    n            = n(),
    emp_state_m  = round(mean(emp_state, na.rm = TRUE), 2),   # felt sympathy, mean
    emp_state_sd = round(sd(emp_state, na.rm = TRUE), 2),     # felt sympathy, spread
    ident_m      = round(mean(ident_state, na.rm = TRUE), 2), # perspective-taking, mean
    ident_sd     = round(sd(ident_state, na.rm = TRUE), 2),   # perspective-taking, spread
    .groups = "drop"
  )

expl_state
# On 1-6 respondents per cell: state sympathy is highest for Narrative (m = 5.00) and
# Social Norm (4.75), not Empathy (4.50); perspective-taking peaks for Social Norm
# (4.25) over Narrative (4.17). SDs are large / undefined (n = 1 Neutral cell). The
# intended manipulation pattern does NOT cleanly emerge - nothing testable here.


# ---- D. H5 sensitivity: GA4 cells attributed to Namibia ----------------------
# The campaign targeted users in Namibia aged 18-35. GA4 nevertheless attributed
# six of the 62 sessions to other countries. Country attribution cannot establish
# whether these records reflect target spillover, geolocation error, internal tests,
# or automated traffic. The unplanned restriction therefore serves only as a
# target-alignment sensitivity analysis. It uses the confirmatory H5 pipeline and
# the same >= 3 s screen.

# Rebuild the H5 set on Namibia-only traffic AT THE UNIT GA4 ACTUALLY MEASURES.
h5_nam_cells <- ga4 %>%
  filter(country == "Namibia",
         condition %in% c("Neutral", "Narrative"),
         !is.na(avg_session_duration_s), sessions > 0,
         avg_session_duration_s >= 3) %>%             # accidental-click screen
  transmute(condition, sessions, cell_duration_s = avg_session_duration_s)

h5_nam_cells %>% count(condition, wt = sessions, name = "sessions")
h5_nam_cells %>% count(condition, name = "cells")
# Namibia-only qualifying data: see the printed counts. Both conditions sit far
# below the preregistered n >= 15 sessions per condition, and the CELL counts that
# carry the inference are smaller still -> descriptive only, no inferential weight.

narr   <- filter(h5_nam_cells, condition == "Narrative")$cell_duration_s  # H5 test group
neut   <- filter(h5_nam_cells, condition == "Neutral")$cell_duration_s    # control group
w_narr <- filter(h5_nam_cells, condition == "Narrative")$sessions         # session weights
w_neut <- filter(h5_nam_cells, condition == "Neutral")$sessions

# Frequency-weighted quantile, DESCRIPTIVE ONLY: rep() reproduces session weights
# exactly for a quantile, a property of the weighted empirical distribution that
# the degrees-of-freedom objection above does not touch. Never fed to a test.
wtd_quantile <- function(x, w, probs) quantile(rep(x, w), probs = probs, names = FALSE)

# Mann-Whitney U (Wilcoxon rank-sum): nonparametric test of whether Narrative cells
# rank higher than Neutral. alternative = "greater" = one-tailed (matches H5's
# direction); exact = TRUE, available at these cell counts with distinct values.
wt_nam  <- wilcox.test(narr, neut, alternative = "greater", exact = TRUE)
# Rank-biserial r = 2U/(n1*n2) - 1, ranging -1..+1 (0 = no tendency, +1 = every
# Narrative cell outlasts every Neutral one).
rbi_nam <- unname((2 * wt_nam$statistic) / (length(narr) * length(neut)) - 1)

# STRATIFIED percentile bootstrap 95% CI for that rank-biserial r: resample each
# group with replacement AT ITS OBSERVED SIZE, recompute r, take the 2.5th / 97.5th
# percentiles.
boot_rbi <- replicate(n_boot, {
  b_n <- sample(narr, replace = TRUE)                          # resample Narrative cells
  b_u <- sample(neut, replace = TRUE)                          # resample Neutral cells
  ww  <- suppressWarnings(wilcox.test(b_n, b_u, exact = FALSE))  # U on the resample
  (2 * unname(ww$statistic)) / (length(b_n) * length(b_u)) - 1 # same rank-biserial formula
})
ci_nam <- quantile(boot_rbi, c(0.025, 0.975), na.rm = TRUE, names = FALSE)  # 95% CI bounds

h5_nam_result <- tibble(
  analysis         = "H5 Namibia-only sensitivity",
  n_cells_narrative = length(narr), n_cells_neutral = length(neut),
  n_sessions_narrative = sum(w_narr), n_sessions_neutral = sum(w_neut),
  median_narrative = round(wtd_quantile(narr, w_narr, 0.5), 1),   # session-weighted
  median_neutral   = round(wtd_quantile(neut, w_neut, 0.5), 1),
  W                = round(unname(wt_nam$statistic), 0),
  p_one_tailed     = round(wt_nam$p.value, 4),
  rbi              = round(rbi_nam, 3),
  rbi_ci_low       = round(ci_nam[1], 3), rbi_ci_high = round(ci_nam[2], 3),
  status           = "EXPLORATORY sensitivity"
)

h5_nam_result
# Namibia-only: the session-weighted neutral median again EXCEEDS the narrative one
# and the rank-biserial r runs OPPOSITE to H5's prediction with a CI spanning zero
# (printed values above). The Namibia restriction yields no narrative attention
# advantage either, on data far too thin to conclude anything from.

write_csv(h5_nam_result, here("output/tables/study2_expl_h5_namibia.csv"))


# ---- E. Engagement beyond CTR -----------------------------------------------
# Meta delivers richer behaviour than link clicks. Video completion is a proxy for
# sustained attention (transportation-adjacent for Narrative); reactions/shares/
# saves index social engagement; cost metrics feed the practical implications (5.3).

# Pooled behavioural rates per condition (summed over both rounds). All observed
# rates, no model - the raw engagement picture beyond the link click.
expl_engage <- meta %>%
  group_by(condition) %>%
  summarise(
    impressions    = sum(impressions),
    thruplay_rate  = round(sum(thruplays) / sum(impressions), 4),      # thruplays per impression
    p100_per_3s    = round(sum(video_p100) / sum(video_3s_views), 4),  # full-view rate among 3s viewers
    actions_per_1k = round(sum(reactions + comments + shares + saves) /
                             sum(impressions) * 1000, 2),              # social actions per 1,000 impressions
    saves          = sum(saves),                                       # raw save count (rare)
    shares         = sum(shares),                                      # raw share count
    cpc_link_eur   = round(sum(spend) / sum(link_clicks), 3),          # cost per link click (efficiency, 5.3)
    cpm_eur        = round(sum(spend) / sum(impressions) * 1000, 3),   # cost per 1,000 impressions
    .groups = "drop"
  )

expl_engage
# Neutral/Empathy show the highest full-view rate (p100_per_3s ~ .069-.070); Social
# Norm lowest (.042), Narrative .060 - but the two long-video conditions sit lower
# (see confound below), so this is largely a duration artefact, not a framing effect.
# CPC ranges ~EUR 0.12-0.13; CPM ~EUR 0.54-0.56.

write_csv(expl_engage, here("output/tables/study2_expl_engagement.csv"))

# Video completion GLM: full views (p100) per 3-second view, vs. Neutral.
# DURATION CONFOUND: video length co-varies with condition (Neutral/Empathy ~38 s,
# Narrative ~46 s, Social Norm ~52 s - documented deviation, Ch 4 §4.1.3). Longer
# videos are mechanically harder to finish, so lower completion for Social Norm /
# Narrative is at least partly a duration artefact, NOT a framing effect. Plain
# binomial (not quasi-): descriptive side-analysis; the confound matters more than
# dispersion here. cbind(successes, failures) feeds proportion data to the GLM;
# relevel to Neutral so each coefficient reads "condition vs. Neutral".
completion_glm <- meta %>%
  mutate(condition = relevel(condition, ref = "Neutral")) %>%   # Neutral = reference level
  glm(cbind(video_p100, video_3s_views - video_p100) ~ condition,
      data = ., family = binomial(link = "logit")) %>%
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%   # coefficients -> odds ratios + 95% CI
  filter(str_detect(term, "condition")) %>%        # drop intercept; keep the 3 frame contrasts
  transmute(
    condition = str_remove(term, "condition"),     # "conditionEmpathy" -> "Empathy"
    OR = round(estimate, 3),                        # odds of finishing vs. Neutral
    CI_lower = round(conf.low, 3), CI_upper = round(conf.high, 3),
    z = round(statistic, 2), p_two_tailed = round(p.value, 4),   # two-tailed (exploratory)
    status = "EXPLORATORY"
  )

completion_glm
# Completion odds vs. Neutral: Empathy OR = 1.01 (ns), Social Norm OR = 0.585
# (p < .001), Narrative OR = 0.869 (p = .0005). Lower completion for the two
# long-video conditions is consistent with the duration confound, not a framing deficit.

write_csv(completion_glm, here("output/tables/study2_expl_completion_glm.csv"))


# ---- F. CTR decline across rounds: condition x round interaction ------------
# Overall CTR dropped from R1 to R2 (saturation; frequency rose). Did the decline
# differ by condition (is the framing effect round-stable)?

# Two nested CTR models: "main-effects" lets condition and round each shift CTR
# independently...
glm_main <- glm(cbind(link_clicks, impressions - link_clicks) ~ condition + round,
                data = meta, family = binomial(link = "logit"))
# ...the "interaction" model additionally lets the framing effect DIFFER by round.
glm_int  <- glm(cbind(link_clicks, impressions - link_clicks) ~ condition * round,
                data = meta, family = binomial(link = "logit"))
# Likelihood-ratio test: does adding the interaction improve fit beyond chance? A
# non-significant Chisq means the CTR decline is uniform across conditions (framing
# rank order is round-stable), which is the pattern H7 predicts.
lrt <- anova(glm_main, glm_int, test = "Chisq")

# Noncentral-chi-square interval for Cohen's w. The point estimate is the same
# sqrt(chi-square / N) value reported below; the interval makes its uncertainty
# explicit as recommended by JARS.
round_interaction_w <- effectsize::chisq_to_phi(
  lrt$Deviance[2], n = sum(meta$impressions), df = lrt$Df[2],
  ci = 0.95, alternative = "two.sided", adjust = FALSE
)

ctr_round_interaction <- tibble(
  test     = "LRT condition x round (Binomial GLM)",
  df       = lrt$Df[2],
  deviance = round(lrt$Deviance[2], 2),
  p        = round(lrt$`Pr(>Chi)`[2], 4),
  p_exact  = signif(lrt$`Pr(>Chi)`[2], 3),
  # Effect size beside the test, since a deviance difference carries no magnitude.
  # w = sqrt(chi2 / N) on the impressions the model was fitted to, Cohen's
  # conventional chi-square effect size (.10 small, .30 medium, .50 large).
  N        = sum(meta$impressions),
  cohen_w  = round(sqrt(lrt$Deviance[2] / sum(meta$impressions)), 4),
  w_ci_low = round(round_interaction_w$CI_low, 4),
  w_ci_high = round(round_interaction_w$CI_high, 4),
  status   = "EXPLORATORY"
)

ctr_round_interaction
# Chisq(3) = 0.28, p = .963 -> NON-significant. The CTR decline is uniform across
# conditions, so framing rank order is round-stable, consistent with the confirmed
# H7. This homogeneity is what licenses pooling the two rounds in Table 16 and in
# the RQ2 ranking.

write_csv(ctr_round_interaction, here("output/tables/study2_expl_ctr_round_interaction.csv"))


# ---- G. Clicks vs. dwell: ad-level CTR vs. GA4 session duration -------------
# Do ads that attract more clicks also hold visitors longer on the page? Unit =
# the 12 ads (condition x variant), pooled across rounds; GA4 duration =
# session-weighted mean per utm_content.

# Collapse to one CTR per ad, pooling both rounds.
ad_ctr_pooled <- meta %>%
  group_by(condition, variant) %>%
  summarise(ctr = sum(link_clicks) / sum(impressions), .groups = "drop")

# Collapse GA4 to one dwell value per ad: session-weighted mean duration so
# high-traffic sessions count proportionally (not a plain mean of ad means).
ad_dwell <- ga4 %>%
  group_by(condition, variant) %>%
  summarise(
    dwell_s  = weighted.mean(avg_session_duration_s, w = sessions),  # weighted by session count
    sessions = sum(sessions),                                        # total sessions for that ad
    .groups = "drop"
  )

# inner_join keeps only ads present in BOTH tables (ads that got GA4 sessions).
ctr_dwell <- ad_ctr_pooled %>% inner_join(ad_dwell, by = c("condition", "variant"))

# Correlate ad-level CTR with ad-level dwell: do click-magnet ads also hold people?
g_res <- spearman_boot(ctr_dwell$ctr, ctr_dwell$dwell_s)

g_res
# n = 11 ads with GA4 sessions: rho = -.318, 95% CI [-.75, .33], p = .340. No
# reliable association - the ads that pull clicks are not the ads that hold attention.

# The Results quote this coefficient in prose, so its n and p need a written
# source rather than a script comment (APA 7 section 6.44).
write_csv(g_res, here("output/tables/study2_expl_ctr_vs_dwell_corr.csv"))

# Title and note are built from ctr_dwell and g_res rather than typed. A typed
# note had drifted from the coefficient beside it (it read rho = -.39, p = .217
# against the computed -.318, p = .340) and the title said 12 ads where the
# inner_join leaves 11.
dwell_note <- str_c(
  "Spearman rho = ", fmt_r(g_res$rho),
  ", 95% CI [", fmt_r(g_res$ci_low), ", ", fmt_r(g_res$ci_high), "], p ",
  if (g_res$p < .001) "< .001" else str_c("= ", fmt_p(g_res$p)),
  ". CTR and dwell are unrelated at the ad level."
)

write_csv(ctr_dwell %>% mutate(across(where(is.numeric), ~ round(.x, 4))),
          here("output/tables/study2_expl_ctr_vs_dwell.csv"))
save_apa_table(ctr_dwell %>% mutate(across(where(is.numeric), ~ round(.x, 4))),
               "study2_expl_ctr_vs_dwell",
               title = str_c("Table. Ad-Level CTR vs. Dwell Time (Exploratory, n = ",
                             nrow(ctr_dwell), " Ads)"),
               note  = dwell_note,
               digits = 4)


# ---- H. Hook analysis: the first 3 seconds ----------------------------------
# hook_rate = 3-second views per impression (did the opening grab attention?);
# hold_rate = thruplays per 3-second view (did it keep it?). Unlike full completion
# (Section E), the 3-second window is the SAME for all conditions, so the hook
# comparison is NOT duration-confounded.

# Per-condition hook and hold rates (pooled).
expl_hooks <- meta %>%
  group_by(condition) %>%
  summarise(
    impressions = sum(impressions),
    hook_rate   = round(sum(video_3s_views) / sum(impressions), 4),   # stopped the scroll?
    hold_rate   = round(sum(thruplays) / sum(video_3s_views), 4),     # kept those it stopped?
    link_ctr    = round(sum(link_clicks) / sum(impressions), 4),      # eventually clicked?
    .groups = "drop"
  )

expl_hooks
# Hook rate is similar across conditions (~.186-.192): the opening 3 s grabs a fifth
# of impressions regardless of frame. Hold rate ~ .305-.320. Link CTR ~ .0041-.0048.

write_csv(expl_hooks, here("output/tables/study2_expl_hooks.csv"))

# Hook GLM: 3s views per impression, vs. Neutral (duration-fair). The 3-second
# window is identical for every condition, so unlike completion (E) this comparison
# is NOT contaminated by the video-length confound.
hook_glm <- meta %>%
  mutate(condition = relevel(condition, ref = "Neutral")) %>%   # Neutral = reference
  glm(cbind(video_3s_views, impressions - video_3s_views) ~ condition,   # successes = 3s views
      data = ., family = binomial(link = "logit")) %>%
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%   # -> odds ratios + 95% CI
  filter(str_detect(term, "condition")) %>%        # keep the 3 frame-vs-Neutral rows
  transmute(
    condition = str_remove(term, "condition"),
    OR = round(estimate, 3),                        # odds of a 3s view vs. Neutral
    CI_lower = round(conf.low, 3), CI_upper = round(conf.high, 3),
    z = round(statistic, 2), p_two_tailed = round(p.value, 4),
    status = "EXPLORATORY"
  )

# ---- Hook-rate GLM, unadjusted [not reported] --------------------------------
# Three-second views per impression per condition against neutral, without
# adjusting for creative variant or round.
hook_glm
# Empathy OR = 0.978 (p = .044), social norm OR = 1.01 (p = .508),
# narrative OR = 0.967 (p = .003).
# -> leaving variant and round out overstates how certain the differences are. The
#    adjusted model is reported, where narrative and neutral capture viewers at
#    about the same rate, OR = 0.98, 95% CI [0.91, 1.04], p = .453.

write_csv(hook_glm, here("output/tables/study2_expl_hook_glm.csv"))

# Does a stronger hook translate into clicks? Ad-level (n = 12, pooled rounds).
ad_hook <- meta %>%
  group_by(condition, variant) %>%
  summarise(
    hook_rate = sum(video_3s_views) / sum(impressions),   # x-candidate: opening grab
    hold_rate = sum(thruplays) / sum(video_3s_views),     # x-candidate: sustained watching
    link_ctr  = sum(link_clicks) / sum(impressions),      # y: the click outcome
    .groups = "drop"
  )

hook_ctr_corr <- bind_rows(
  spearman_boot(ad_hook$hook_rate, ad_hook$link_ctr) %>% mutate(predictor = "hook_rate"),
  spearman_boot(ad_hook$hold_rate, ad_hook$link_ctr) %>% mutate(predictor = "hold_rate")
) %>%
  relocate(predictor)

hook_ctr_corr
# Ad-level (n = 12): hook_rate x link_CTR rho = .839, 95% CI [.37, .98], p = .0006 -
# ads whose opening grabs more scrolls do convert more clicks. hold_rate x link_CTR
# rho = -.462, 95% CI [-.77, .29], p = .131 - sustained watching does NOT predict CTR.

# NB: these are TWO different tables and they must not share a file stem. An earlier
# version wrote the per-ad rates to study2_expl_hooks_adlevel.csv and the correlation
# results to study2_expl_hooks_adlevel.docx, so the .csv and the .docx of the same
# name carried different content. Distinct stems now: _rates vs. _corr.
# The CSV keeps full precision. Rounding it to 4 decimals ties four of the twelve
# link_ctr values at .0044, and a reader recomputing the correlation from the rounded
# table gets rho = .784 instead of the .839 reported beside it. The rendered APA table
# below still rounds, because that is a display choice, but the machine-readable file
# a reproducer would actually use must not lose the digits the statistic depends on.
write_csv(ad_hook, here("output/tables/study2_expl_hooks_adlevel_rates.csv"))
save_apa_table(ad_hook %>% mutate(across(where(is.numeric), ~ round(.x, 4))),
               "study2_expl_hooks_adlevel_rates",
               title = "Table. Hook Rate, Hold Rate and Link CTR per Advertisement (Exploratory, n = 12)",
               note  = "Hook rate = 3-s views / impressions. Hold rate = ThruPlays / 3-s views.",
               digits = 4)

write_csv(hook_ctr_corr, here("output/tables/study2_expl_hooks_adlevel_corr.csv"))


# ---- I. Variant heterogeneity within conditions -----------------------------
# Each condition ran 3 creative variants. If CTR differs strongly BETWEEN variants
# of the same condition, creative execution matters beyond framing.

# First collapse to one CTR per variant (drop_last keeps the condition grouping),
# then summarise each condition across its 3 variants.
variant_het <- meta %>%
  group_by(condition, variant) %>%
  summarise(clicks = sum(link_clicks), imps = sum(impressions),
            ctr = clicks / imps, .groups = "drop_last") %>%   # still grouped by condition
  summarise(
    ctr_min       = round(min(ctr), 4),                # lowest-CTR variant
    ctr_max       = round(max(ctr), 4),                # highest-CTR variant
    max_min_ratio = round(max(ctr) / min(ctr), 2),     # spread as a fold-difference
    # prop.test on the 3 variants' click/impression counts: k-sample test of equal
    # proportions (chi-square of homogeneity). Small p = variants differ, so creative
    # execution moves CTR beyond framing alone.
    chisq_p = round(prop.test(x = clicks, n = imps)$p.value, 4),
    .groups = "drop"
  )

variant_het
# Within-condition variant spread is real: max/min CTR ratio 1.30-1.56, and prop.test
# rejects homogeneity in every condition (chisq_p .0007-.0496). Creative execution
# moves CTR at least as much as framing does.

write_csv(variant_het, here("output/tables/study2_expl_variant_heterogeneity.csv"))


# ---- J. GA4 on-page engagement by condition ---------------------------------
# Session-weighted engagement rate and engagement time per condition. The
# confirmatory H5 only compares Narrative vs. Neutral, so these numbers are context.

expl_ga4 <- ga4 %>%
  group_by(condition) %>%
  summarise(
    dwell_s         = round(weighted.mean(avg_session_duration_s, w = sessions), 1),  # mean session length (s)
    # Session-weighted median, the figure the RQ2 ranking in the thesis quotes. The
    # mean above is pulled up by a few very long visits, so the two run in different
    # directions for Social Norm. wtd_quantile() is the helper defined in Section E.
    dwell_median_s  = round(wtd_quantile(avg_session_duration_s, sessions, 0.50), 1),
    engagement_rate = round(weighted.mean(engagement_rate, w = sessions), 3),         # GA4 "engaged" share
    engage_time_s   = round(weighted.mean(avg_engagement_time_s, w = sessions), 1),   # active on-page time (s)
    n_sessions      = sum(sessions),   # summarise is sequential - keep last so
    .groups = "drop"                   # `sessions` stays row-level above
  )

# Interval for the session-weighted mean. The chapter quotes one for the social-norm
# condition, so it has to come out of the pipeline. The resampling unit is the CELL,
# not the session, for the same reason the H5 test is run on cells: GA4 returns cell
# means and session counts, never individual sessions, so a session-level resample
# would invent variance that was never observed. The RNG stream is saved and restored
# so no interval computed later in this script moves.
.rng_before_ga4 <- .Random.seed
set.seed(2026)
ga4_mean_ci <- ga4 %>%
  filter(!is.na(avg_session_duration_s), sessions > 0) %>%
  group_by(condition) %>%
  group_modify(function(d, key) {
    draws <- replicate(20000, {
      i <- sample(nrow(d), nrow(d), replace = TRUE)
      weighted.mean(d$avg_session_duration_s[i], d$sessions[i])
    })
    q <- quantile(draws, c(0.025, 0.975), names = FALSE, na.rm = TRUE)
    tibble(n_cells = nrow(d), n_sessions = sum(d$sessions),
           dwell_mean_s = round(weighted.mean(d$avg_session_duration_s, d$sessions), 1),
           ci_lo = round(q[1], 1), ci_hi = round(q[2], 1))
  }) %>%
  ungroup()
.Random.seed <- .rng_before_ga4

ga4_mean_ci
# The social-norm interval is very wide because two of its nine cells carry long
# visits. It is reported to show that the 120 s mean is not pinned down, not to
# support a comparison.

write_csv(ga4_mean_ci, here("output/tables/study2_expl_ga4_mean_ci.csv"))

expl_ga4
# On-page dwell varies on tiny session counts (10-18 sessions/condition): Neutral
# 45.4 s, Empathy 41.9 s, Narrative 43.5 s, Social Norm 120 s (a few long outliers).
# Narrative and Neutral (the H5 pair) sit close, echoing the H5 null.
# The medians run the other way and rank Social Norm last: Neutral 36.6 s, Empathy
# 22.4 s, Narrative 20.2 s, Social Norm 14.5 s. These four are the values the RQ2
# paragraph reports. NB: this is the all-cells sample, whereas the H5 test in
# 04_confirmatory.R is restricted to the cells meeting the preregistered qualifying
# criterion, which is why the neutral figure there (52.8 s) is the larger one.

write_csv(expl_ga4, here("output/tables/study2_expl_ga4_engagement.csv"))


# ---- K. Delivery demographics (age x gender per condition) ------------------
# From pull_extras.py (Marketing API breakdowns). JARS asks for demographic
# comparability of delivery: with algorithmic (non-random) delivery, a demographic
# skew that differs by condition is a confound, not a result.

# The demographic CSVs are optional; glob for them and branch on whether any exist.
demo_files <- list.files(here("data/raw/MetaAds"),
                         pattern = "^MetaAds_demographics_", full.names = TRUE)

if (length(demo_files) == 0) {
  # No breakdown files present: skip Sections K + L (leave `demo` undefined).
  demo <- NULL
} else {
  # Read + row-bind all matching CSVs, then label each row's condition by
  # pattern-matching the ad-name slug (which encodes the frame).
  demo <- demo_files %>%
    map_dfr(~ read_csv(.x, show_col_types = FALSE)) %>%
    mutate(condition = factor(case_when(
      str_detect(ad_name, "neutral")    ~ "Neutral",
      str_detect(ad_name, "empathy")    ~ "Empathy",
      str_detect(ad_name, "socialnorm") ~ "Social Norm",
      str_detect(ad_name, "narrative")  ~ "Narrative"
    ), levels = cond_levels))

  # Gender split of who Meta SERVED each condition to. drop_last leaves the condition
  # grouping so imp_share sums within condition.
  demo_gender <- demo %>%
    filter(gender %in% c("female", "male")) %>%   # drop "unknown"/other for a clean 2-way split
    group_by(condition, gender) %>%
    summarise(impressions = sum(impressions),
              link_clicks = sum(link_clicks), .groups = "drop_last") %>%
    mutate(imp_share = round(impressions / sum(impressions), 3),   # share of condition's impressions
           ctr       = round(link_clicks / impressions, 4)) %>%    # CTR within this gender stratum
    ungroup()

  demo_gender
  # Delivery leans slightly MALE in every condition (male imp_share ~ .53-.55), and
  # the split is comparable ACROSS conditions - a uniform delivery property, not a
  # per-condition confound. Female CTR runs above male within every stratum
  # (e.g. Neutral .0049 vs .0034).

  write_csv(demo_gender, here("output/tables/study2_expl_demographics_gender.csv"))
  save_apa_table(demo_gender, "study2_expl_demographics_gender",
                 title = "Table. Delivery by Gender within Condition (Exploratory)",
                 note  = "imp_share = share of the condition's impressions. Slightly male-skewed delivery, comparable across conditions.",
                 digits = 4)

  # Same idea across Meta's age bands: is delivery age-skewed between conditions?
  demo_age <- demo %>%
    group_by(condition, age) %>%
    summarise(impressions = sum(impressions),
              link_clicks = sum(link_clicks), .groups = "drop_last") %>%
    mutate(imp_share = round(impressions / sum(impressions), 3),
           ctr       = round(link_clicks / impressions, 4)) %>%
    ungroup()

  demo_age
  # Age delivery concentrates in 18-24 (imp_share ~ .57-.59) and 25-34 (~ .40), with
  # only ~2% in 35-44 - a comparable profile across conditions, again a uniform
  # delivery property rather than a per-condition confound.

  write_csv(demo_age, here("output/tables/study2_expl_demographics_age.csv"))
  save_apa_table(demo_age, "study2_expl_demographics_age",
                 title = "Table. Delivery by Age Band within Condition (Exploratory)",
                 note  = "imp_share = share of the condition's impressions. Age profile comparable across conditions.",
                 digits = 4)

  # Round-to-round comparability. The two blocks above ask whether delivery differed
  # BETWEEN CONDITIONS; this one asks whether it differed BETWEEN ROUNDS. APA Ch. 3
  # Table 6 asks a replication report to compare the demographics of the replicated
  # deliveries, and H7 claims the CTR rank order held across the two rounds, which is
  # interpretable only if the rounds reached similar audiences. Table 16 pools them,
  # so this is also the check that licenses the pooling.
  demo_round <- bind_rows(
      demo %>% filter(gender %in% c("female", "male")) %>%
        transmute(round, stratum = "gender", level = gender, impressions),
      demo %>% transmute(round, stratum = "age", level = age, impressions)
    ) %>%
    group_by(stratum, round, level) %>%
    summarise(impressions = sum(impressions), .groups = "drop") %>%
    group_by(stratum, round) %>%
    mutate(imp_share = impressions / sum(impressions)) %>%   # share within stratum x round
    ungroup() %>%
    dplyr::select(-impressions) %>%
    pivot_wider(names_from = round, values_from = imp_share, names_prefix = "round_") %>%
    mutate(diff_pp = round(100 * (round_1 - round_2), 1),
           across(starts_with("round_"), ~ round(.x, 3))) %>%
    arrange(stratum, level)

  # ---- Demographics by round [not reported] ----------------------------------
  # Gender and age delivery split by round, to see whether the two rounds reached
  # similar audiences.
  demo_round
  max(abs(demo_round$diff_pp), na.rm = TRUE)
  # Largest gap is 2.0 points on the female share (.451 against .471), age bands
  # differ by at most 1.4.
  # -> the two rounds reached almost the same audience, so pooling them hides no
  #    shift. Delivery by condition, pooled across rounds, is reported in Table 11.

  write_csv(demo_round, here("output/tables/study2_expl_demographics_by_round.csv"))
}


# ---- L. H2a via Meta demographics (gender x empathy framing) ----------------
# Non-preregistered exploratory. Exit-survey H2a was not estimable (tiny Empathy n).
# Alternative operationalisation: Meta ad-level demographic breakdown (gender of
# served impressions), independent of exit-survey participation.
#   (a) Descriptive Empathy-Neutral CTR contrast by gender (condition-level).
#   (b) Spearman rho: per-ad % female vs. CTR within Empathy ads (N = 6 obs:
#       3 ads x 2 rounds). Both reported as exploratory only.

if (!is.null(demo)) {

  # Collapse the demographics to one row per ad x round x gender (age bands summed).
  demo_ad_gender <- demo %>%
    filter(gender %in% c("female", "male")) %>%
    group_by(round, ad_name, condition, gender) %>%
    summarise(impressions = sum(impressions),
              link_clicks = sum(link_clicks), .groups = "drop") %>%
    mutate(ctr = link_clicks / impressions)   # gender-stratum CTR for this ad-round

  # Reshape to one row per Empathy ad-round with female/male columns side by side,
  # so % female of delivery and the ad's overall CTR can be computed per observation.
  emp_demo_wide <- demo_ad_gender %>%
    filter(condition == "Empathy") %>%
    pivot_wider(names_from  = gender,                          # female/male -> separate columns
                values_from = c(impressions, link_clicks, ctr)) %>%
    mutate(
      pct_female = impressions_female / (impressions_female + impressions_male),  # share of impressions to women
      ctr_total  = (link_clicks_female + link_clicks_male) /
                   (impressions_female + impressions_male)      # pooled CTR of the ad
    )

  # (a) Descriptive: Empathy-Neutral CTR contrast by gender. pivot so Neutral and
  # Empathy CTRs sit in columns, then compute the gap (does empathy lift CTR more
  # for women than men, as H2a predicts?). Descriptive, not a test.
  h2a_contrast <- demo_gender %>%
    filter(condition %in% c("Neutral", "Empathy")) %>%
    dplyr::select(condition, gender, ctr) %>%
    pivot_wider(names_from = condition, values_from = ctr) %>%
    mutate(
      ctr_diff_pp = round((Empathy - Neutral) * 100, 4),   # empathy lift in percentage points
      ctr_ratio   = round(Empathy / Neutral, 4)            # empathy lift as a multiplier
    )

  h2a_contrast
  # The empathy-vs-Neutral CTR gap is tiny and runs the WRONG way for H2a: women
  # -0.01 pp (ratio 0.98), men +0.04 pp (ratio 1.12). If anything empathy helps men
  # more - no female-specific empathy advantage.

  write_csv(h2a_contrast, here("output/tables/study2_expl_h2a_gender_contrast.csv"))

  # (b) Spearman rho: % female vs. total CTR within Empathy ads (N = 6). Guard
  # N >= 3 (correlation undefined below that). Treats gender as the preregistered
  # moderator via Meta delivery, independent of exit-survey response.
  if (nrow(emp_demo_wide) >= 3) {
    rho_h2a <- cor.test(emp_demo_wide$pct_female, emp_demo_wide$ctr_total,
                       method = "spearman", exact = FALSE)   # rank correlation, asymptotic p
    h2a_spearman <- tibble(
      n    = nrow(emp_demo_wide),
      rho  = round(as.numeric(rho_h2a$estimate), 3),
      p    = round(rho_h2a$p.value, 3),
      note = "pct_female vs. total CTR within empathy ads (per ad x round)"
    )
  } else {
    h2a_spearman <- tibble(n = nrow(emp_demo_wide), rho = NA_real_, p = NA_real_,
                          note = "not estimable (N < 3)")
  }

  h2a_spearman
  # N = 6 Empathy ad-rounds: rho = -.143, p = .787. No association between how female
  # an Empathy ad's delivery was and its CTR - the delivery-based H2a proxy is null too.

}
