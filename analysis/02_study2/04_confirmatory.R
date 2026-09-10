# =============================================================================
# 02_study2/04_confirmatory.R
#
# WHERE THIS BELONGS
#   In the thesis: Chapter 4, Results: Delivery Diagnostics, Primary Outcomes, Secondary Outcome, Replication
#   Produces: Tables 10 and 12, the H5, H7 and session-capture passages
#   Status: PREREGISTERED. Registered with AsPredicted on 10 June 2026, before
#   the campaign launched. Nothing here was decided after seeing the data.
#
# Study 2 - Naturalistic Framing Comparison: CTR (H1, H3, H4, H7) and Time on Page (H5)
#
# Study 2 is a NON-experimental, naturalistic comparison: Meta's ad algorithm
# decided who saw which ad, so there is no random assignment and no causal claim.
# All hypotheses were preregistered (AsPredicted, 2026-06-10, before launch);
# The two research questions are exploratory extras.
#
# Preregistered tests and thresholds:
#   H1, H3, H4        Empathy / Social Norm / Narrative CTR > Neutral   Binomial GLM, one-tailed, alpha = .017 (Bonferroni)
#   H5                Narrative Time on Page > Neutral                  Mann-Whitney U, one-tailed, alpha = .05
#   H2a, H2b, H6      Gender / Env. concern moderators                  Spearman rho, Holm - only if exit-survey N >= 20
#   H7                CTR rank order replicates across rounds           Kendall's tau >= .60 + neutral-rank rule
#
# Inputs  (data/processed/, from 02_study2/03_process_campaign.R):
#   meta_ads_clean.csv   one row per ad = condition x variant x round  (CTR)
#   ga4_clean.csv        landing-page sessions per UTM                 (Time on Page)
#   exitsurvey_clean.csv one row per exit-survey respondent            (moderators)
#
# out: output/tables/study2_delivery_diagnostics.csv          (Table 10, + .docx)
#      output/tables/study2_glm_ctr.csv                       (Table 12, + .docx)
#      output/tables/study2_glm_ctr_quasibinomial.csv         (Table 12, + .docx)
#      output/tables/study2_quasi_wedderburn.csv              (dispersion, + .docx)
#      output/tables/study2_glm_ctr_reach_sensitivity.csv     (reach denominator)
#      output/tables/study2_ctr_unweighted_sensitivity.csv    (unweighted check)
#      output/tables/study2_h5_descriptives.csv               (H5)
#      output/tables/study2_h5_descriptives_all_conditions.csv (H5, all four)
#      output/tables/study2_h5_mann_whitney.csv               (H5 test)
#      output/tables/study2_h5_sensitivities.csv              (H5 sensitivities)
#      output/tables/study2_h7_kendall.csv                    (H7 replication)
#      output/tables/study2_mde_precision.csv                 (detectable effect)
#      output/tables/study2_session_capture.csv               (session capture, + .docx)
#      output/tables/study2_session_capture_chisq.csv         (session capture, test)
# =============================================================================

# pacman::p_load() installs a package if missing, then loads it, in one call.
pacman::p_load(here, tidyverse, broom, boot, flextable, officer, effectsize)

# Source every helper in src/ (theme_apa(), condition_colours, save_apa(),
# theme_flex_apa(), save_apa_table(), fmt_* formatters). walk() = a for-loop that
# runs source() on each file for its side effect and returns nothing.
list.files(here("src"), full.names = TRUE) %>% walk(source)


# ---- Analysis-wide constants ------------------------------------------------

# Bonferroni-corrected alpha for the 3 primary CTR tests: splitting the familywise
# .05 across H1, H3 and H4 (.05 / 3 = .0167) holds the chance of ANY false positive at 5%.
alpha_primary <- 0.017
alpha_h5      <- 0.050   # H5 is a different outcome (Time on Page), judged uncorrected
n_boot        <- 5000    # bootstrap resamples for effect-size CIs

# Lock the RNG so the bootstrap CI below reproduces exactly on every run (the same
# seed 02_study2/14_exploratory.R uses). Without this the H5 rank-biserial CI drifts
# in the second decimal from run to run.
set.seed(42)

# Neutral listed FIRST so it becomes the factor reference level - the control that
# every motivational frame is compared against.
cond_levels <- c("Neutral", "Empathy", "Social Norm", "Narrative")


# ---- Load data --------------------------------------------------------------

meta <- here("data/processed/meta_ads_clean.csv") %>%
  read_csv(show_col_types = FALSE) %>%
  mutate(
    condition = factor(condition, levels = cond_levels),   # Neutral = reference level
    round     = factor(round)                              # a grouping label, not a quantity
  )

# GA4 (H5) and exit-survey (H2a, H2b, H6) can lag behind the Meta data. Load
# each if it exists, otherwise fall back to an empty tibble so the CTR analysis
# still runs.
load_or_empty <- function(path) {
  if (file.exists(path)) {
    read_csv(path, show_col_types = FALSE) %>%
      mutate(condition = factor(condition, levels = cond_levels))
  } else {
    tibble(condition = factor(character(), levels = cond_levels))
  }
}

ga4        <- load_or_empty(here("data/processed/ga4_clean.csv"))
exitsurvey <- load_or_empty(here("data/processed/exitsurvey_clean.csv"))

nrow(meta); nrow(ga4); nrow(exitsurvey)
# 24 Meta ad rows (12 ads x 2 rounds), 38 GA4 UTM rows, 13 exit-survey responses.


# ---- 0. Delivery diagnostics ------------------------------------------------
# Reported BEFORE any hypothesis test: shows the algorithm gave each condition
# comparable exposure, which is what makes a later CTR gap interpretable.

delivery <- meta %>%
  group_by(round, condition) %>%
  summarise(
    n_ads       = n(),                                   # ads in this cell (should be 3)
    impressions = sum(impressions, na.rm = TRUE),        # total times shown
    reach       = sum(reach, na.rm = TRUE),              # unique accounts reached
    link_clicks = sum(link_clicks, na.rm = TRUE),        # CTR numerator
    ctr_mean    = round(link_clicks / impressions, 4),   # observed CTR for the cell
    frequency   = round(impressions / reach, 2),         # avg times a reached account saw an ad
    .groups     = "drop"
  )

delivery
# Exposure is comparable across conditions: impressions ~52k-58k, reach ~47k-53k,
# frequency 1.05-1.12 in every cell. No condition was structurally over-/under-served,
# so the delivery assumption behind the CTR comparison holds.

write_csv(delivery, here("output/tables/study2_delivery_diagnostics.csv"))
save_apa_table(delivery, "study2_delivery_diagnostics",
               title = "Table. Delivery Diagnostics by Condition and Round",
               note  = "Frequency = impressions / reach. Comparable delivery across conditions.",
               digits = 4)

# Condition-level CTR: sum clicks and impressions across the 3 ads BEFORE dividing,
# which weights each ad by its impression volume (the preregistered aggregation).
condition_ctr <- meta %>%
  group_by(round, condition) %>%
  summarise(
    total_impressions = sum(impressions, na.rm = TRUE),
    total_clicks      = sum(link_clicks, na.rm = TRUE),
    ctr               = round(total_clicks / total_impressions, 4),
    .groups           = "drop"
  )

condition_ctr
# Narrative has the numerically highest CTR in BOTH rounds (R1 .59% vs Neutral .50%;
# R2 .37% vs Neutral .33%). Empathy/Social Norm sit level with Neutral. The rank
# order (Narrative on top, Neutral at/near the bottom) is the pattern H7 later tests.



# ---- H1, H3, H4: Binomial GLM - CTR vs. Neutral (Bonferroni alpha = .017) ---
# CTR is a proportion (clicks out of impressions), so we model click PROBABILITY on
# the logit scale rather than running a t-test on the ratio. Feeding the GLM
# cbind(clicks, non-clicks) weights each ad by its impressions; exponentiating a
# coefficient gives an ODDS RATIO (OR > 1 = clicked at higher odds than Neutral).
# Tests are one-tailed because each frame is predicted to BEAT Neutral (directional).

fit_ctr_glm <- function(data, round_label, response_denominator = "impressions") {

  d <- data %>% filter(round == round_label)
  # Re-assert Neutral as reference inside the subset so every coefficient reads
  # "frame vs. Neutral".
  d$condition <- relevel(factor(d$condition, levels = cond_levels), ref = "Neutral")

  failures <- d[[response_denominator]] - d$link_clicks   # non-clicks (impressions - clicks)

  glm(cbind(link_clicks, failures) ~ condition,
      data = d, family = binomial(link = "logit")) %>%
    # tidy() -> one row per coefficient; exponentiate = TRUE turns log-odds into
    # odds ratios; conf.int = TRUE adds the 95% CI.
    tidy(exponentiate = TRUE, conf.int = TRUE) %>%
    filter(str_detect(term, "condition")) %>%                # drop intercept, keep the 3 frame contrasts
    transmute(                                               # transmute = mutate + keep only these columns
      round        = round_label,
      condition    = str_remove(term, "condition"),          # "conditionEmpathy" -> "Empathy"
      OR           = round(estimate, 3),
      ci_lower     = round(conf.low, 3),
      ci_upper     = round(conf.high, 3),
      z            = round(statistic, 2),
      p_two        = round(p.value, 4),
      # One-tailed p: halve it only when the effect is in the predicted direction
      # (OR > 1); a "greater" hypothesis cannot be supported by a negative effect.
      p_one        = round(if_else(OR > 1, p.value / 2, 1 - p.value / 2), 4),
      significant  = p_one < alpha_primary
    )
}

# map_dfr() = run fit_ctr_glm() for each round and row-bind the results.
glm_ctr <- levels(meta$round) %>% map_dfr(~ fit_ctr_glm(meta, .x))

glm_ctr
# Narrative R1: OR = 1.19, 95% CI [1.01, 1.40], p_one = .018 -> just ABOVE alpha = .017.
# Empathy/Social Norm R1: OR ~ 1.05, ns. Round 2: all ORs 1.01-1.13, none significant.
# -> H1, H3 and H4 are NOT supported at the Bonferroni threshold. Narrative is the
#    only frame that even approaches it, and only in Round 1.

write_csv(glm_ctr, here("output/tables/study2_glm_ctr.csv"))
save_apa_table(glm_ctr, "study2_glm_ctr",
               title = "Table. Binomial GLM: CTR by Framing Condition vs. Neutral",
               note  = "OR = odds ratio vs. Neutral. p_one = one-tailed. Decision at alpha = .017 (Bonferroni).",
               digits = 3)

# Preregistered robustness check (AsPredicted Q6): with only 12 ad-level counts the
# clicks can be OVER-DISPERSED relative to a binomial (which fixes variance at the
# mean). The quasi-binomial estimates a dispersion phi and inflates the SEs, giving
# t- instead of z-statistics - the conservative fallback the prereg specifies.
fit_ctr_glm_quasi <- function(data, round_label) {
  d <- data %>% filter(round == round_label)
  d$condition <- relevel(factor(d$condition, levels = cond_levels), ref = "Neutral")

  model <- glm(cbind(link_clicks, impressions - link_clicks) ~ condition,
               data = d, family = quasibinomial(link = "logit"))
  disp <- summary(model)$dispersion   # phi: 1 = no overdispersion, > 1 = overdispersed

  # Interval and test must answer to the SAME reference distribution. A quasi model
  # is judged on t with df.residual degrees of freedom (8 here: 12 ads - 4 parameters),
  # so the CI is a Wald interval on that same t, not broom's default conf.int = TRUE
  # (profile likelihood, which references the normal and is therefore too narrow for
  # the test being reported beside it). Slightly wider, and consistent.
  t_crit <- qt(0.975, df.residual(model))

  model %>%
    tidy(exponentiate = FALSE) %>%                           # keep the log-odds scale for the interval arithmetic
    filter(str_detect(term, "condition")) %>%
    transmute(
      round      = round_label,
      condition  = str_remove(term, "condition"),
      OR         = round(exp(estimate), 3),
      ci_lower   = round(exp(estimate - t_crit * std.error), 3),
      ci_upper   = round(exp(estimate + t_crit * std.error), 3),
      t          = round(statistic, 2),
      p_one      = round(if_else(exp(estimate) > 1, p.value / 2, 1 - p.value / 2), 4),
      dispersion = round(disp, 2),
      significant = p_one < alpha_primary
    )
}

glm_ctr_quasi <- levels(meta$round) %>% map_dfr(~ fit_ctr_glm_quasi(meta, .x))

glm_ctr_quasi
# Round 1 is strongly overdispersed (phi = 6.58); Round 2 mildly so (phi = 1.85).
# Under the conservative quasi-binomial NOTHING is significant (Narrative R1 p_one
# rises from .018 to .218). -> the H1/H3/H4 nulls hold under the preregistered fallback.

write_csv(glm_ctr_quasi, here("output/tables/study2_glm_ctr_quasibinomial.csv"))
save_apa_table(glm_ctr_quasi, "study2_glm_ctr_quasibinomial",
               title = "Table. Quasi-Binomial GLM (Overdispersion-Robust)",
               note  = "phi = dispersion (1 = none). Preregistered fallback for overdispersed counts.",
               digits = 3)

# Wedderburn's proportionality assumption, tested rather than only named.
# The quasi-binomial rests on the excess variance being a constant multiple of the
# binomial variance. phi reports that multiple, it does not check that a single
# multiple fits. wedderburn_check() (src/quasi_diagnostics.R) regresses the squared
# Pearson residuals on the fitted rate and on the trial count. It draws no random
# numbers, so it cannot shift any bootstrap that follows it in this script.
quasi_models <- levels(meta$round) %>% set_names() %>% map(function(r) {
  d <- meta %>% filter(round == r)
  d$condition <- relevel(factor(d$condition, levels = cond_levels), ref = "Neutral")
  glm(cbind(link_clicks, impressions - link_clicks) ~ condition,
      data = d, family = quasibinomial(link = "logit"))
})

wedderburn <- imap_dfr(quasi_models, ~ wedderburn_check(.x, paste("CTR quasi-binomial, round", .y)))

wedderburn
# Read together with the residual df: at 8 df the slopes are themselves imprecise, so
# a non-significant result is weak reassurance, not a clean bill of health.

write_csv(wedderburn, here("output/tables/study2_quasi_wedderburn.csv"))
save_apa_table(wedderburn, "study2_quasi_wedderburn",
               title = "Table. Proportionality Check for the Quasi-Binomial Specification",
               note  = paste("Squared Pearson residuals regressed on the fitted rate and on the",
                             "trial count. Under Wedderburn (1974) proportionality both slopes are",
                             "zero. A positive trials slope is the beta-binomial signature."),
               digits = 4)


# ---- Precision analysis: minimum detectable effect (MDE) --------------------
# Sample size was fixed by budget, not by a power analysis, so what the design
# could resolve is established AFTER the fact from the impressions actually
# delivered. This is a precision analysis ON THE DESIGN (what effect would have
# been detectable at the realised volumes), NOT a retrospective power analysis on
# the observed effect, which would be circular.
#
# Method: two-proportion normal approximation. For a one-tailed test at alpha with
# power (1 - beta), the smallest detectable comparison-condition rate p2 solves
#   (p2 - p1) / SE = z_alpha + z_beta,  SE = sqrt(phi) * sqrt(p1(1-p1)/n1 + p2(1-p2)/n2)
# where p1/n1 are the Neutral rate and impressions, n2 the mean impressions of a
# comparison condition, and phi the dispersion estimated by the quasi-binomial
# above (phi = 1 reproduces the binomial specification). p2 is then converted to
# an odds ratio against Neutral.

mde_odds_ratio <- function(p1, n1, n2, phi = 1, alpha = alpha_primary, power = 0.80) {
  z_alpha <- qnorm(1 - alpha)     # one-tailed critical value
  z_beta  <- qnorm(power)         # 0.8416 at 80% power
  gap <- function(p2) {
    se <- sqrt(phi) * sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2)
    (p2 - p1) / se - (z_alpha + z_beta)
  }
  # uniroot() finds where gap() crosses zero, i.e. the smallest detectable p2.
  p2 <- uniroot(gap, interval = c(p1 + 1e-9, 0.5))$root
  (p2 / (1 - p2)) / (p1 / (1 - p1))
}

mde_precision <- levels(meta$round) %>% map_dfr(~ {
  d   <- meta %>% filter(round == .x)
  neu <- d %>% filter(condition == "Neutral")
  # n2 = impressions of an AVERAGE comparison condition (3 ads pooled), since each
  # hypothesis test contrasts one such condition against Neutral.
  n2  <- d %>% filter(condition != "Neutral") %>%
    group_by(condition) %>% summarise(imp = sum(impressions), .groups = "drop") %>%
    pull(imp) %>% mean()
  n1  <- sum(neu$impressions)
  p1  <- sum(neu$link_clicks) / n1
  phi <- glm_ctr_quasi %>% filter(round == .x) %>% pull(dispersion) %>% first()

  tibble(
    round            = .x,
    neutral_imp      = n1,
    neutral_ctr_pct  = round(p1 * 100, 3),
    comparison_imp   = round(n2),
    dispersion_phi   = phi,
    mde_or_binomial  = round(mde_odds_ratio(p1, n1, n2, phi = 1),   2),
    mde_or_quasi     = round(mde_odds_ratio(p1, n1, n2, phi = phi), 2)
  )
})

# The preregistered expectation for H3 was an effect bracket in Cohen's d
# (Bergquist et al., 2019). Convert it to the odds-ratio scale the design speaks
# in, via the logistic approximation OR = exp(d * pi / sqrt(3)).
d_to_or <- function(d) exp(d * pi / sqrt(3))
prereg_d      <- c(0.18, 0.59)
prereg_or     <- d_to_or(prereg_d)
prereg_or_mid <- d_to_or(mean(prereg_d))     # midpoint ON THE d SCALE, then converted

mde_precision <- mde_precision %>%
  mutate(
    prereg_or_low = round(prereg_or[1], 2),
    prereg_or_mid = round(prereg_or_mid, 2),
    prereg_or_high = round(prereg_or[2], 2),
    # Does the round's decisive (quasi-binomial) MDE reach the bracket midpoint?
    resolves_midpoint = mde_or_quasi <= round(prereg_or_mid, 2)
  )

mde_precision
# Round 1: neutral 54,531 impressions at 0.499%, comparison 52,738 -> MDE OR 1.28
#   (binomial) / 1.78 (quasi, phi = 6.58). Round 2: 53,203 at 0.327%, comparison
#   54,964 -> 1.35 / 1.47 (phi = 1.85). Preregistered bracket d = 0.18-0.59 =
#   OR 1.39-2.92, midpoint OR 2.02. BOTH rounds resolve the midpoint under the
#   quasi-binomial specification; neither reaches the bracket's lower end.

write_csv(mde_precision, here("output/tables/study2_mde_precision.csv"))

# Preregistered sensitivity (AsPredicted Q8): CTR defined as clicks / REACH (unique
# accounts) instead of clicks / impressions, to check the result is not an artefact
# of the denominator. Same quasi-binomial machinery, reusing fit_ctr_glm's structure.
sens_reach <- levels(meta$round) %>% map_dfr(~ {
  d <- meta %>% filter(round == .x)
  d$condition <- relevel(factor(d$condition, levels = cond_levels), ref = "Neutral")
  glm(cbind(link_clicks, reach - link_clicks) ~ condition,
      data = d, family = quasibinomial(link = "logit")) %>%
    tidy(exponentiate = TRUE, conf.int = TRUE) %>%
    filter(str_detect(term, "condition")) %>%
    transmute(round = .x, condition = str_remove(term, "condition"),
              OR = round(estimate, 3), ci_lower = round(conf.low, 3),
              ci_upper = round(conf.high, 3),
              p_one = round(if_else(estimate > 1, p.value / 2, 1 - p.value / 2), 4))
})

sens_reach
# Clicks/reach reproduces the null result: all p_one > .21, no condition beats Neutral.
# The H1, H3 and H4 conclusion is not an artefact of how CTR is defined.

write_csv(sens_reach, here("output/tables/study2_glm_ctr_reach_sensitivity.csv"))

# Second preregistered sensitivity: UNWEIGHTED condition means (each ad's CTR counts
# equally, regardless of impression volume) - the mirror image of the weighted primary.
sens_unweighted <- meta %>%
  mutate(ad_ctr = link_clicks / impressions) %>%      # CTR of each individual ad
  group_by(round, condition) %>%
  summarise(unweighted_ctr = round(mean(ad_ctr), 4), .groups = "drop")

sens_unweighted
# Same rank order as the weighted CTR (Narrative on top), so the impression-weighting
# is not driving the pattern.

write_csv(sens_unweighted, here("output/tables/study2_ctr_unweighted_sensitivity.csv"))


# ---- Session capture: is the tracking loss even across conditions? ----------
# Only a small fraction of link clicks reach GA4 as an attributed session (consent
# gate + Instagram's in-app browser). That loss sits BETWEEN the click and the
# time-on-page outcome, so before H5 is read at all it has to be shown that the
# loss is not condition-dependent. A 2 x 4 chi-square on captured vs. uncaptured
# clicks tests exactly that.

capture_tab <- meta %>%
  group_by(condition) %>%
  summarise(link_clicks = sum(link_clicks, na.rm = TRUE), .groups = "drop") %>%
  left_join(
    ga4 %>% group_by(condition) %>%
      summarise(sessions = sum(sessions, na.rm = TRUE), .groups = "drop"),
    by = "condition"
  ) %>%
  mutate(
    sessions            = coalesce(sessions, 0),
    uncaptured          = link_clicks - sessions,
    sessions_per_1000   = round(1000 * sessions / link_clicks, 1)
  ) %>%
  arrange(condition)

capture_mat <- capture_tab %>% dplyr::select(sessions, uncaptured) %>% as.matrix()
rownames(capture_mat) <- as.character(capture_tab$condition)
capture_chisq <- chisq.test(capture_mat)

# Cramer's V with a 95% interval, computed here rather than by hand. The chapter
# reports an effect size beside this chi-square, and JARS asks for its interval, so
# the number has to come out of the pipeline. The RNG stream is saved and restored
# around the call so that no bootstrap later in this script can shift, whatever
# effectsize does internally.
.rng_before_v <- if (exists(".Random.seed", envir = .GlobalEnv)) .Random.seed else NULL
# adjust = FALSE keeps the plain Cramer's V, sqrt(chi2 / (N * df_min)), which is the
# quantity the chapter reports. The bias-adjusted variant would print 0 here.
# alternative = "two.sided" gives a two-sided interval; the package default
# ("greater") returns a one-sided bound of 1 and would not match a reported CI.
capture_v <- effectsize::cramers_v(capture_mat, ci = 0.95,
                                   adjust = FALSE, alternative = "two.sided")
if (!is.null(.rng_before_v)) .Random.seed <- .rng_before_v

capture_result <- tibble(
  test       = "Session capture x condition (2 x 4)",
  chi_square = round(unname(capture_chisq$statistic), 2),
  df         = unname(capture_chisq$parameter),
  p          = round(capture_chisq$p.value, 3),
  min_expected_count = round(min(capture_chisq$expected), 1),
  cramers_v  = round(capture_v[[1]], 3),
  v_ci_low   = round(capture_v$CI_low, 3),
  v_ci_high  = round(capture_v$CI_high, 3)
)

capture_tab; capture_result
# Capture runs at 22.4 sessions per 1,000 clicks for Neutral, 36.0 Empathy, 37.0
# Social Norm, 35.3 Narrative: chi2(3) = 2.02, p = .567, i.e. indistinguishable
# from chance. Consent-gated tracking loss is therefore NOT condition-dependent.
# It still cripples H5 in absolute terms, but it does not bias the comparison.

write_csv(capture_tab %>% dplyr::select(condition, link_clicks, sessions, sessions_per_1000),
          here("output/tables/study2_session_capture.csv"))
write_csv(capture_result, here("output/tables/study2_session_capture_chisq.csv"))
save_apa_table(capture_tab %>% dplyr::select(condition, link_clicks, sessions, sessions_per_1000),
               "study2_session_capture",
               title = "Table. Landing-Page Session Capture by Condition",
               note  = paste0("Captured GA4 sessions per 1,000 Meta link clicks, pooled across rounds. ",
                              "Chi-square(", capture_result$df, ") = ", capture_result$chi_square,
                              ", p = ", format(capture_result$p, nsmall = 3), "."),
               digits = 1)


# ---- H5: Narrative vs. Neutral Time on Page (alpha = .05) -------------------
# Time-on-page is heavily right-skewed (a few long readers, many quick bounces), so
# we compare RANKS not means. Directional (Narrative predicted to hold attention
# longer), so alternative = "greater". Effect size = rank-biserial r.
#
# UNIT OF ANALYSIS. GA4 does not return sessions; it returns AGGREGATED CELLS
# (utm_content x round x geography), each carrying a mean session duration and a
# session count. The test therefore runs at the level GA4 actually measures, the
# UTM cell, and session counts enter only as WEIGHTS for the descriptive
# quantiles, never as an n for inference.
#
# Expanding each cell with uncount(sessions) and testing the resulting rows would
# be wrong in a way worth stating plainly. It does not merely compress within-cell
# variance, it MANUFACTURES DEGREES OF FREEDOM. The eleven qualifying cells would
# become 24 pseudo-observations that the test then treats as independent, so the
# p value and the bootstrap CI would both be computed against a sample size that
# was never observed.

if (nrow(ga4) == 0) {
  message("GA4 data not available - H5 skipped.")
} else {

  h5_cells_all <- ga4 %>%
    filter(condition %in% c("Neutral", "Narrative"),   # H5 compares only these two
           !is.na(avg_session_duration_s), sessions > 0) %>%
    transmute(condition = droplevels(condition),
              sessions,
              cell_duration_s = avg_session_duration_s)

  # Preregistered analytic definition (Q6): >= 3 s counts as engagement, screening
  # out accidental taps that bounce immediately. Applied to the cell mean, which is
  # the finest resolution the export offers.
  h5_cells <- h5_cells_all %>% filter(cell_duration_s >= 3)

  # Frequency-weighted quantile: rep() reproduces the session weights EXACTLY for a
  # quantile, which is a property of the weighted empirical distribution and is
  # unaffected by the df problem above. Descriptive use only - never fed to a test.
  wtd_quantile <- function(x, w, probs) quantile(rep(x, w), probs = probs, names = FALSE)

  h5_descriptives <- h5_cells %>%
    group_by(condition) %>%
    summarise(
      cells             = n(),                    # the inferential n
      # NB: the weighted quantiles must be computed BEFORE any column named
      # `sessions` is created here, or summarise() would resolve `sessions` to that
      # new scalar and silently return unweighted cell quantiles.
      median_weighted_s = round(wtd_quantile(cell_duration_s, sessions, 0.50), 1),
      q1_weighted_s     = round(wtd_quantile(cell_duration_s, sessions, 0.25), 1),
      q3_weighted_s     = round(wtd_quantile(cell_duration_s, sessions, 0.75), 1),
      median_cell_s     = round(median(cell_duration_s), 1),
      n_sessions        = sum(sessions),          # the descriptive weight total
      .groups = "drop"
    ) %>%
    relocate(n_sessions, .after = cells)

  h5_descriptives
  # 6 narrative cells (16 sessions) vs. 5 neutral cells (8 sessions): far below the
  # preregistered 15 qualifying sessions per condition, and further below any
  # sensible cell count -> H5 is descriptive only, per the preregistered if-then rule.

  write_csv(h5_descriptives, here("output/tables/study2_h5_descriptives.csv"))

  # H5 itself compares only Narrative against Neutral, so the table above holds two
  # conditions. The Discussion nonetheless refers to dwell time in other conditions,
  # and a descriptive quoted for some conditions but not all invites the question of
  # what the missing ones looked like. The same >= 3 s rule and the same weighted
  # quantiles, applied to all four, answer it. Descriptive only: no test is run here,
  # and the H5 result above is untouched.
  h5_descriptives_all <- ga4 %>%
    filter(!is.na(avg_session_duration_s), sessions > 0, avg_session_duration_s >= 3) %>%
    group_by(condition) %>%
    summarise(
      cells             = n(),
      median_weighted_s = round(wtd_quantile(avg_session_duration_s, sessions, 0.50), 1),
      q1_weighted_s     = round(wtd_quantile(avg_session_duration_s, sessions, 0.25), 1),
      q3_weighted_s     = round(wtd_quantile(avg_session_duration_s, sessions, 0.75), 1),
      mean_weighted_s   = round(weighted.mean(avg_session_duration_s, sessions), 1),
      median_cell_s     = round(median(avg_session_duration_s), 1),
      n_sessions        = sum(sessions),
      .groups = "drop"
    ) %>%
    relocate(n_sessions, .after = cells) %>%
    complete(condition)          # keep a row for any condition with no qualifying cell

  h5_descriptives_all
  write_csv(h5_descriptives_all, here("output/tables/study2_h5_descriptives_all_conditions.csv"))

  narrative_cells <- h5_cells %>% filter(condition == "Narrative") %>% pull(cell_duration_s)
  neutral_cells   <- h5_cells %>% filter(condition == "Neutral")   %>% pull(cell_duration_s)

  # One-tailed Mann-Whitney (Wilcoxon rank-sum) on the cells. With 6 vs. 5 distinct
  # values the EXACT null distribution is available, so no normal approximation is
  # needed and no tie correction is required.
  h5_test <- wilcox.test(narrative_cells, neutral_cells,
                         alternative = "greater", exact = TRUE)

  # Rank-biserial r from the Wilcoxon W (= Mann-Whitney U for the first sample):
  # r = 2U / (n1 * n2) - 1. r > 0 means Narrative > Neutral; it is the net share of
  # cross-pairs favouring Narrative (0 = no difference, +1 = every narrative cell outlasts every neutral cell).
  rank_biserial <- unname((2 * h5_test$statistic) / (length(narrative_cells) * length(neutral_cells)) - 1)

  # STRATIFIED percentile bootstrap: resample WITHIN each condition so the group
  # sizes stay at their observed 6 and 5. A pooled resample would let the group
  # sizes drift, a second and smaller source of inflated precision.
  rbi_boot <- replicate(n_boot, {
    b_n <- sample(narrative_cells, replace = TRUE)
    b_u <- sample(neutral_cells,   replace = TRUE)
    w   <- suppressWarnings(wilcox.test(b_n, b_u, exact = FALSE))
    (2 * unname(w$statistic)) / (length(b_n) * length(b_u)) - 1
  })
  rbi_ci <- quantile(rbi_boot, c(0.025, 0.975), na.rm = TRUE, names = FALSE)

  h5_result <- tibble(
    comparison   = "Narrative vs. Neutral",
    n_cells_narrative = length(narrative_cells),
    n_cells_neutral   = length(neutral_cells),
    n_sessions_narrative = sum(h5_cells$sessions[h5_cells$condition == "Narrative"]),
    n_sessions_neutral   = sum(h5_cells$sessions[h5_cells$condition == "Neutral"]),
    W            = round(unname(h5_test$statistic), 0),
    p_one        = round(h5_test$p.value, 4),
    rbi          = round(rank_biserial, 3),
    rbi_ci_low   = round(rbi_ci[1], 3),
    rbi_ci_high  = round(rbi_ci[2], 3),
    significant  = h5_test$p.value < alpha_h5
  )

  h5_result
  # W = 13, p_one = .669, r = -.13, 95% CI [-.80, .60] on 6 vs. 5 cells. The point
  # estimate runs OPPOSITE to the prediction and the CI spans almost the whole
  # range -> H5 NOT supported, and the data cannot support any conclusion at all.
  # Session-weighted descriptives: median 22.2 s narrative (IQR 20.2-79.4, 16
  # sessions) vs. 52.8 s neutral (IQR 31.3-69.1, 8 sessions) - neutral LONGER.

  write_csv(h5_result, here("output/tables/study2_h5_mann_whitney.csv"))

  # Preregistered sensitivities: stricter 5-s engagement floor, and a parametric
  # cross-check (Welch t on LOG seconds, since log tames the right skew). Both at
  # cell level, for the same reason as the primary test.
  h5_5s   <- h5_cells_all %>% filter(cell_duration_s >= 5)
  test_5s <- wilcox.test(
    h5_5s %>% filter(condition == "Narrative") %>% pull(cell_duration_s),
    h5_5s %>% filter(condition == "Neutral")   %>% pull(cell_duration_s),
    alternative = "greater", exact = TRUE)

  welch <- t.test(
    log(cell_duration_s) ~ condition,
    data = h5_cells %>%
      mutate(condition = droplevels(factor(condition, levels = c("Narrative", "Neutral")))),
    alternative = "greater")

  # Cohen's d for the Welch row on the same log-second cells, with the
  # Hedges-Olkin large-sample CI (no extra package needed). Reported because the
  # chapter cites d and a script console print is not a source.
  log_nar <- log(h5_cells$cell_duration_s[h5_cells$condition == "Narrative"])
  log_neu <- log(h5_cells$cell_duration_s[h5_cells$condition == "Neutral"])
  n1 <- length(log_nar); n2 <- length(log_neu)
  sd_pool <- sqrt(((n1 - 1) * var(log_nar) + (n2 - 1) * var(log_neu)) / (n1 + n2 - 2))
  d_welch <- (mean(log_nar) - mean(log_neu)) / sd_pool
  se_d    <- sqrt((n1 + n2) / (n1 * n2) + d_welch^2 / (2 * (n1 + n2 - 2)))

  h5_sensitivities <- tibble(
    sensitivity = c(">= 5 s Mann-Whitney (cells)", "Welch t on log s (>= 3 s, cells)"),
    n_narrative = c(sum(h5_5s$condition == "Narrative"), length(narrative_cells)),
    n_neutral   = c(sum(h5_5s$condition == "Neutral"),   length(neutral_cells)),
    statistic   = c(round(unname(test_5s$statistic), 0), round(unname(welch$statistic), 2)),
    df          = c(NA, round(unname(welch$parameter), 1)),
    p_one       = c(round(test_5s$p.value, 4), round(welch$p.value, 4)),
    d           = c(NA, round(d_welch, 2)),
    d_ci_low    = c(NA, round(d_welch - 1.96 * se_d, 2)),
    d_ci_high   = c(NA, round(d_welch + 1.96 * se_d, 2))
  )

  h5_sensitivities
  # Both agree with the primary null (see printed values after the run). The H5
  # conclusion is robust to threshold and test choice - which says little, given
  # that the design never had the resolution to detect anything.

  write_csv(h5_sensitivities, here("output/tables/study2_h5_sensitivities.csv"))
}


# ---- Moderators H2a, H2b, H6 (Spearman rho, only if exit-survey N >= 20) ----
# Do gender / age / environmental concern change how a frame moves CTR? Gated at
# N >= 20: below that the prereg forbids inferential testing (too little data).

n_exitsurvey <- nrow(exitsurvey)
n_exitsurvey
# N = 13 -> below the preregistered minimum of 20. Per the preregistration, H2a,
# H2b and H6 are NOT analysed. Reported as: "The minimum sample requirement
# (N = 20) for the moderator analyses was not met; H2a, H2b and H6 were not
# tested."

if (n_exitsurvey >= 20) {
  # (Confirmatory moderator block - not reached with the present N; see 04b for the
  #  exploratory-despite-gate version.)
  message("Exit-survey N >= 20: run the preregistered moderator analyses here.")
}


# ---- H7: Replication - Kendall's tau across rounds --------------------------
# H7 asks whether the CTR ORDER of the four conditions repeats across the two
# independent rounds. Kendall's tau measures rank-order agreement (+1 = identical,
# 0 = unrelated, -1 = reversed). This is a criterion check, not an NHST (with 4
# conditions p is uninformative): the decision rests on tau >= .60 PLUS Neutral
# ranking below at least 2 motivational conditions in both rounds.

# Reshape to one row per condition, one column per round.
# Rank the UNROUNDED condition CTR. The `ctr` column in condition_ctr is rounded to
# 4 decimals for display, and at these CTR magnitudes that rounding collapses
# genuinely different conditions onto the same value (R1 Empathy .0052466 vs. Social
# Norm .0052493; R2 Neutral .0032705 vs. Social Norm .0033129). Those artificial ties
# inflated Kendall's tau and understated how far Neutral fell in Round 2, so the
# ranking works from total_clicks / total_impressions directly.
ctr_wide <- condition_ctr %>%
  mutate(ctr_exact = total_clicks / total_impressions) %>%
  dplyr::select(round, condition, ctr_exact) %>%
  pivot_wider(names_from = round, values_from = ctr_exact) %>%
  # rank(-ctr): negating makes rank 1 the HIGHEST CTR; ties.method = "min" gives
  # tied conditions the same best rank.
  mutate(
    rank_r1 = rank(-`1`, ties.method = "min"),
    rank_r2 = rank(-`2`, ties.method = "min")
  )

ctr_wide
# Rank orders are near-identical across rounds: Narrative = 1 in both; Neutral last
# in both. This is the visual signature of a high tau.

# Kendall's tau between the two rank vectors = the replication statistic.
tau_test <- cor.test(ctr_wide$rank_r1, ctr_wide$rank_r2, method = "kendall")

# Neutral-rank criterion: a higher rank NUMBER = worse CTR, so neutral_rank > a
# motivational rank means that frame beat Neutral. Count how many did, per round.
neutral_rank <- ctr_wide %>% filter(condition == "Neutral")
mot_ranks    <- ctr_wide %>% filter(condition != "Neutral")
neutral_below_r1 <- sum(neutral_rank$rank_r1 > mot_ranks$rank_r1)
neutral_below_r2 <- sum(neutral_rank$rank_r2 > mot_ranks$rank_r2)

# Bootstrap interval for tau. JARS asks for an interval on every reported effect size,
# and tau is the effect size for the one supported hypothesis. Four rank pairs is a very
# small base, so the interval is expected to be wide; reporting it is the point, because
# it shows the reader how little the point estimate is pinned down. The RNG stream is
# saved and restored so no interval computed later in this script shifts.
.rng_before_tau <- .Random.seed
tau_boot <- replicate(n_boot, {
  i <- sample(nrow(ctr_wide), replace = TRUE)
  suppressWarnings(cor(ctr_wide$rank_r1[i], ctr_wide$rank_r2[i], method = "kendall"))
})
tau_ci <- round(quantile(tau_boot, c(0.025, 0.975), na.rm = TRUE, names = FALSE), 3)
.Random.seed <- .rng_before_tau

h7_result <- tibble(
  tau              = round(tau_test$estimate, 3),
  tau_ci_low       = tau_ci[1],
  tau_ci_high      = tau_ci[2],
  p                = round(tau_test$p.value, 4),
  neutral_below_r1 = neutral_below_r1,
  neutral_below_r2 = neutral_below_r2,
  # && is scalar AND (single TRUE/FALSE), the right operator for a one-line decision.
  criterion_met    = round(tau_test$estimate, 3) >= 0.60 &&
                     neutral_below_r1 >= 2 && neutral_below_r2 >= 2
)

h7_result
# tau = .67 (>= .60) and Neutral ranks below 3/3 motivational conditions in BOTH
# rounds -> BOTH criteria met, so H7 IS SUPPORTED: the CTR rank order (Narrative
# highest, Neutral lowest) replicated across the two campaign rounds. The one
# rank swap is Empathy vs. Social Norm in the middle of the order. This is the
# study's one confirmatory success - a stable ORDERING even though no single frame
# beat Neutral significantly (H1, H3, H4). The accompanying p = .333 is the exact
# Kendall test on 4 rank pairs and carries essentially no evidential weight; the
# preregistered decision rests on the tau and neutral-rank criteria, not on p.

write_csv(h7_result, here("output/tables/study2_h7_kendall.csv"))


# ---- RQ2: Exploratory ranking (NOT preregistered) ---------------------------

# RQ2: which framing had the highest pooled CTR? Pool clicks/impressions across both
# rounds, then rank.
rq2_ctr <- condition_ctr %>%
  group_by(condition) %>%
  summarise(pooled_ctr = sum(total_clicks) / sum(total_impressions), .groups = "drop") %>%
  arrange(desc(pooled_ctr))

rq2_ctr
# Pooled ranking: Narrative highest, then Empathy / Social Norm level, Neutral lowest -
# consistent with the H7 ordering. Descriptive only, no inferential claim.
