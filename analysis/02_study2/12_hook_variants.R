# =============================================================================
# 02_study2/12_hook_variants.R
#
# WHERE THIS BELONGS
#   In the thesis: Chapter 4, Results: Exploratory Analyses
#   Produces: Table 14
#   Status: Exploratory. Study 2 has a preregistration, but this script is not
#   part of it. Only 04_confirmatory.R and 05_confirmatory_h2a.R are, and any
#   result here is a finding to be retested rather than a test.
#
# Study 2 - EXPLORATORY hook-variant analysis (NOT preregistered).
#
# Everything here is EXPLORATORY and not preregistered; all tests two-tailed and
# must be reported as exploratory in the thesis (Ch 4). No causal claim.
#
# Question: which of the three shared hook clips captures the most attention, and
# does framing act AFTER capture? Raw CTR mixes two stages: (1) does the ad stop
# the scroll at all (the hook clip's job) and (2) once watched, does the framing
# convert the viewer to a click. Because the same three hook clips are reused
# across all four framing conditions, comparing hook clips isolates stage (1), and
# modelling clicks-per-3s-view isolates stage (2).
#
# Design background (Appendix C): Section A hook footage is identical across the
# four framing conditions of a given variant and differs only BETWEEN variants
# (V1 = original single landscape shot; V2 = dry canyon -> cracked clay pan;
# V3 = dune sea -> rugged mountains). Variant identity is orthogonal to framing by
# construction, and the 3-s view falls inside Section A (0-5 s), so
# hook_rate = video_3s_views / impressions indexes the hook clip (plus auction
# noise), not the framing manipulation.
#
# Contents:
#   A. Variant-level descriptives (hook rate, CTR, post-hook conversion)
#   B. GLMs, binomial + quasi-binomial (quasi = robust spec, cf. 02_study2/04_confirmatory.R):
#      B1 hook rate ~ variant + condition + round
#      B2 link CTR  ~ variant + condition + round
#      B3 post-hook conversion (clicks per 3-s view) ~ condition + variant + round
#   C. Ordering stability: variant ranks per round + per condition-round cell
#
# NOTE on B3: link clicks are not formally nested within 3-s viewers (a user can
# click without a counted 3-s play), so clicks per 3-s view is a rate approximation,
# not a true conditional probability, and failures = video_3s_views - link_clicks
# can go negative (would break the binomial cbind). Guarded before the fit.
#
# Inputs (data/processed/, from 02_study2/03_process_campaign.R):
#   meta_ads_clean.csv   one row per ad = condition x variant x round
#
# Run order: 02_study2/03_process_campaign.R -> this script (independent of 04 / 04b).
#
# out: output/tables/study2_expl_hookvariant_rates.csv        (Table 14, + .docx)
#      output/tables/study2_expl_hookvariant_glm.csv          (Table 14, + .docx)
#      output/tables/study2_expl_hookvariant_omnibus.csv      (omnibus, + .docx)
#      output/tables/study2_expl_hookvariant_cellwinners.csv  (cell winners)
#      output/tables/study2_expl_twostage_condition_or.csv    (post-hook conversion)
#      output/tables/study2_expl_posthook_conversion_glm.csv  (B3 model, + .docx)
# =============================================================================

# pacman::p_load() installs a package if missing, then loads it, in one call.
pacman::p_load(here, tidyverse, broom, effectsize, flextable, officer)

# Source every helper in src/ (theme_apa(), condition_colours, save_apa(),
# save_apa_table(), fmt_* formatters). walk() runs source() on each file for its
# side effect and returns nothing.
list.files(here("src"), full.names = TRUE) %>% walk(source)


# ---- Analysis-wide constants ------------------------------------------------

# Neutral listed FIRST so it becomes the factor reference level in the GLMs - the
# control the framing contrasts are compared against.
cond_levels <- c("Neutral", "Empathy", "Social Norm", "Narrative")


# ---- Load data --------------------------------------------------------------

# Cleaned per-ad Meta export; one row per ad (condition x variant x round).
# condition/variant/round become factors so the GLMs treat them as categorical.
meta <- here("data/processed/meta_ads_clean.csv") %>%
  read_csv(show_col_types = FALSE) %>%
  mutate(
    condition = factor(condition, levels = cond_levels),
    variant   = factor(variant),
    round     = factor(round)
  )

# Sanity gate: the design is 12 ads (4 conditions x 3 variants) across 2 rounds
# = 24 rows. stopifnot() aborts immediately if the input is not that shape.
stopifnot(nrow(meta) == 24)   # 12 ads x 2 rounds


# ---- A. Variant-level descriptives ------------------------------------------

# Stack two copies of the data: one relabelled "Pooled" (both rounds together) and
# one keeping the real round label, so the same summarise() yields both the pooled
# and the per-round descriptives in one table.
variant_rates <- bind_rows(
  meta %>% mutate(round = "Pooled"),
  meta %>% mutate(round = as.character(round))
) %>%
  group_by(round, variant) %>%
  summarise(
    ads          = n(),                                              # ads behind this row
    impressions  = sum(impressions),
    views_3s     = sum(video_3s_views),
    link_clicks  = sum(link_clicks),
    hook_rate    = round(sum(video_3s_views) / sum(impressions), 4), # stage 1: 3-s views per impression (capture)
    link_ctr     = round(sum(link_clicks) / sum(impressions), 4),    # overall CTR
    click_per_3s = round(sum(link_clicks) / sum(video_3s_views), 4), # stage 2: clicks per 3-s view (post-hook)
    .groups = "drop"
  ) %>%
  arrange(factor(round, levels = c("Pooled", "1", "2")), variant)    # Pooled first, then round 1, round 2

variant_rates
# Pooled hook rate is FAR from equal: V1 = .248 vs V2 = .160, V3 = .164 - the
# original single landscape clip (V1) captures markedly more scrolls than the two
# swapped clips. Pooled CTR follows the same order: V1 .0051 > V3 .0044 > V2 .0036.

write_csv(variant_rates, here("output/tables/study2_expl_hookvariant_rates.csv"))
save_apa_table(variant_rates, "study2_expl_hookvariant_rates",
               title = "Table. Hook-Variant Descriptives (V = Shared Hook Clip)",
               note  = "hook_rate = 3s views / impressions; click_per_3s = post-hook conversion. Pooled + per round.",
               digits = 4)


# ---- B. GLMs: binomial + quasi-binomial -------------------------------------
# The quasi-binomial re-estimates standard errors under overdispersion and is
# treated as the robust specification, mirroring the primary analysis logic.

# Fit BOTH a binomial and a quasi-binomial GLM of the same proportion model and
# return their coefficients side by side (exponentiated -> odds ratios). The
# quasi-binomial estimates a dispersion phi and widens SEs when the ad-level counts
# spread more than a pure binomial expects; phi is carried through.
glm_pair <- function(formula, data) {
  fit_b <- glm(formula, data = data, family = binomial())        # standard binomial (dispersion fixed at 1)
  fit_q <- glm(formula, data = data, family = quasibinomial())   # same model, dispersion estimated
  phi   <- summary(fit_q)$dispersion                             # phi: 1 = none, > 1 = overdispersed

  # Interval and test must answer to the SAME reference distribution. The quasi
  # model is judged on t with df.residual degrees of freedom, so its CI is a Wald
  # interval on that same t, NOT broom's conf.int = TRUE (profile likelihood, which
  # references the normal and is therefore too narrow beside the p reported next to
  # it). 02_study2/04_confirmatory.R uses the same Wald-on-t interval. The
  # binomial row keeps broom's default, which is correct for a fixed dispersion.
  t_crit <- qt(0.975, df.residual(fit_q))

  bind_rows(
    tidy(fit_b, exponentiate = TRUE, conf.int = TRUE) %>%
      mutate(spec = "binomial"),
    tidy(fit_q, exponentiate = FALSE) %>%                        # log-odds scale for the interval arithmetic
      mutate(conf.low  = exp(estimate - t_crit * std.error),
             conf.high = exp(estimate + t_crit * std.error),
             estimate  = exp(estimate),
             spec      = "quasibinomial")
  ) %>%
    filter(term != "(Intercept)") %>%                            # keep the predictor contrasts, drop the intercept
    transmute(term, spec,                                        # transmute = build a clean output row
              OR       = round(estimate, 3),                     # odds ratio for this contrast
              CI_lower = round(conf.low, 3),
              CI_upper = round(conf.high, 3),
              p_two_tailed = round(p.value, 4),                  # two-tailed (exploratory: no directional claim)
              dispersion   = round(phi, 2))
}

# Omnibus test for one predictor under quasi-binomial (F test). Refit the model
# without `drop_term` and compare to the full model with an analysis-of-deviance F
# test (F, not chi-square, because the quasi-binomial has an estimated dispersion).
# Answers "does this predictor matter overall?" across all its levels at once.
quasi_F <- function(formula, drop_term, data) {
  fit_full <- glm(formula, data = data, family = quasibinomial())
  fit_red  <- update(fit_full, as.formula(paste(". ~ . -", drop_term)))   # same model minus the dropped predictor
  av <- anova(fit_red, fit_full, test = "F")
  f_val <- av$F[2]; d1 <- av$Df[2]; d2 <- av$`Resid. Df`[2]
  # Partial eta squared, the share of the variance left over by the other predictors
  # that this one accounts for: eta2p = F * df1 / (F * df1 + df2). An F ratio on its
  # own carries no magnitude, so JARS asks for the effect size beside it.
  # F_to_eta2() adds the 95% interval from the noncentral F distribution. That
  # interval is one-sided by construction, so its lower bound can rest at 0.
  e2 <- effectsize::F_to_eta2(f_val, d1, d2, ci = 0.95)
  tibble(term = drop_term,
         F = round(f_val, 2), df1 = d1, df2 = d2,
         p = round(av$`Pr(>F)`[2], 4),
         # signif() keeps three significant digits however small p gets, where
         # round(, 4) would collapse a very small p to a bare 0.
         p_exact       = signif(av$`Pr(>F)`[2], 3),
         eta2_partial  = round(e2$Eta2_partial, 3),
         eta2_ci_lower = round(e2$CI_low,  3),
         eta2_ci_upper = round(e2$CI_high, 3))
}

# B1 - hook rate: does the shared hook clip drive initial capture? Response = 3-s
# views out of impressions; predictors = variant (the hook clip), adjusted for
# condition and round. A variant effect here = the clip mattered for stopping the
# scroll, independent of framing.
f_hook <- cbind(video_3s_views, impressions - video_3s_views) ~
  variant + condition + round

b1   <- glm_pair(f_hook, meta) %>% mutate(outcome = "hook_rate")
b1_F <- quasi_F(f_hook, "variant", meta) %>% mutate(outcome = "hook_rate")   # omnibus test of the variant effect

# Pairwise V3 vs V2: with variant 1 as the default reference the model only gives
# V2-vs-V1 and V3-vs-V1; releveling to V2 exposes the V3-vs-V2 contrast directly.
b1_pair <- glm_pair(f_hook, meta %>% mutate(variant = relevel(variant, "2"))) %>%
  filter(term == "variant3") %>%
  mutate(term = "variant3_vs_variant2", outcome = "hook_rate")

# B2 - link CTR: does the hook clip carry through to clicks? Same predictors, but
# the outcome is link clicks out of impressions. If a clip lifts hook rate AND CTR,
# its attention advantage survives to the click.
f_ctr <- cbind(link_clicks, impressions - link_clicks) ~
  variant + condition + round

b2   <- glm_pair(f_ctr, meta) %>% mutate(outcome = "link_ctr")
b2_F <- quasi_F(f_ctr, "variant", meta) %>% mutate(outcome = "link_ctr")
b2_pair <- glm_pair(f_ctr, meta %>% mutate(variant = relevel(variant, "2"))) %>%
  filter(term == "variant3") %>%
  mutate(term = "variant3_vs_variant2", outcome = "link_ctr")

# Combine the B1 + B2 coefficient tables; relocate() just puts `outcome` first.
hookvariant_glm <- bind_rows(b1, b1_pair, b2, b2_pair) %>%
  relocate(outcome)

hookvariant_glm %>% filter(str_starts(term, "variant"))
# Both swapped clips capture far LESS than V1: hook-rate OR = 0.580 (V2) and 0.596
# (V3), p < .001 even under the robust quasi-binomial. On CTR V2 is clearly worse
# (OR = 0.716, quasi p = .0009), V3 intermediate (OR = 0.873, quasi p = .104), and
# V3 beats V2 (OR = 1.22, quasi p = .032).

hookvariant_omnibus <- bind_rows(b1_F, b2_F)

hookvariant_omnibus
# Omnibus variant effect (quasi-binomial F): hook_rate F(2, 17) = 264, p < .001;
# link_ctr F(2, 17) = 8.26, p = .003. The hook CLIP strongly moves both capture and
# clicks once condition and round are held constant - variant, not framing, is the
# dominant driver here.

write_csv(hookvariant_glm, here("output/tables/study2_expl_hookvariant_glm.csv"))
save_apa_table(hookvariant_glm, "study2_expl_hookvariant_glm",
               title = "Table. Variant GLMs (Reference: Variant 1; Adjusted for Condition and Round)",
               note  = "Binomial + quasi-binomial ORs. Two-tailed (exploratory). dispersion = quasi-binomial phi.",
               digits = 3)
write_csv(hookvariant_omnibus, here("output/tables/study2_expl_hookvariant_omnibus.csv"))
save_apa_table(hookvariant_omnibus, "study2_expl_hookvariant_omnibus",
               title = "Table. Omnibus Variant Effect (Quasi-Binomial F Test)",
               note  = "F test for the overall variant effect on hook rate and CTR, adjusted for condition and round.",
               digits = 3)

# B3 - post-hook conversion: clicks per 3-s view. If framing operates AFTER
# attention capture, condition contrasts should be at least as large here as on raw
# CTR (which dilutes them with the capture stage that Section A dominates). Response
# = clicks out of 3-s VIEWS, so the denominator is "people the hook already caught";
# a condition effect here is framing acting after capture. condition listed first =
# the focal predictor. Guard: a negative failures cell (clicks > 3-s views) would
# break the binomial cbind - assert it cannot happen before fitting.
stopifnot(all(meta$video_3s_views - meta$link_clicks >= 0))   # no negative binomial cells

f_conv <- cbind(link_clicks, video_3s_views - link_clicks) ~
  condition + variant + round

b3   <- glm_pair(f_conv, meta) %>% mutate(outcome = "clicks_per_3s_view")
b3_F <- quasi_F(f_conv, "condition", meta) %>%                # omnibus: does framing move post-hook conversion?
  mutate(outcome = "clicks_per_3s_view")

b3
# Post-hook conversion odds vs. Neutral: Narrative largest (OR = 1.20, binomial
# p = .006) but only p = .075 under the robust quasi-binomial; Empathy/Social Norm
# ~ 1.0. The hook clip again dominates (variant3 OR = 1.32, quasi p = .003) and
# round 2 depresses conversion (OR = 0.708).

b3_F
# Omnibus condition effect on conversion (quasi-binomial F): F(3, 17) = 1.42,
# p = .271 - framing does not reliably move clicks-per-3s-view either.

write_csv(bind_rows(b3, b3_F %>% mutate(term = paste0("OMNIBUS_", term))),
          here("output/tables/study2_expl_posthook_conversion_glm.csv"))
save_apa_table(b3, "study2_expl_posthook_conversion_glm",
               title = "Table. Post-Hook Conversion (Clicks per 3-s View; Ref: Neutral)",
               note  = "Binomial + quasi-binomial ORs vs. Neutral. Framing effect after attention capture.",
               digits = 3)

# Side-by-side: condition ORs on raw CTR vs. on post-hook conversion. The two-stage
# payoff: if a frame's OR is small on raw CTR but larger on post-hook conversion,
# its effect was masked by the attention-capture stage.
ctr_cond  <- glm_pair(f_ctr, meta) %>%
  filter(str_starts(term, "condition")) %>% mutate(outcome = "link_ctr")   # framing ORs on raw CTR
conv_cond <- b3 %>% filter(str_starts(term, "condition"))                   # framing ORs on post-hook conversion
two_stage <- bind_rows(ctr_cond, conv_cond) %>% relocate(outcome)

two_stage
# Condition ORs are nearly identical on raw CTR and on post-hook conversion (e.g.
# Narrative 1.17 vs 1.20, both ns under the quasi-binomial), so framing's small edge
# is NOT hidden by the capture stage - the two stages tell the same weak story.

write_csv(two_stage, here("output/tables/study2_expl_twostage_condition_or.csv"))


# ---- C. Ordering stability --------------------------------------------------

# Rank the three variants within each round on hook rate and on CTR (rank 1 = best).
# Do the same clips win in both rounds? Consistent ranks = a stable hook effect
# rather than round-specific noise.
variant_ranks <- meta %>%
  group_by(round, variant) %>%
  summarise(hook_rate = sum(video_3s_views) / sum(impressions),
            link_ctr  = sum(link_clicks) / sum(impressions),
            .groups = "drop") %>%
  group_by(round) %>%
  mutate(hook_rank = rank(-hook_rate),                       # negate so rank 1 = highest hook rate
         ctr_rank  = rank(-link_ctr)) %>%
  ungroup() %>%
  mutate(across(c(hook_rate, link_ctr), ~ round(.x, 4))) %>% # round both rate columns for display
  arrange(round, hook_rank)

# For every condition x round cell, which variant had the top hook rate / CTR?
# which.max() returns the position of the maximum, used to index the winning variant.
cell_winners <- meta %>%
  mutate(hook_rate = video_3s_views / impressions,
         ad_ctr    = link_clicks / impressions) %>%
  group_by(round, condition) %>%
  summarise(hook_winner = variant[which.max(hook_rate)],
            ctr_winner  = variant[which.max(ad_ctr)],
            .groups = "drop")

variant_ranks
# V1 ranks #1 on BOTH hook rate and CTR in BOTH rounds - the original clip wins
# consistently, so the capture ordering is round-STABLE, matching the significant
# B1/B2 omnibus. (V2 and V3 swap the #2/#3 CTR slots between rounds.)

cell_winners %>% count(hook_winner)   # how many of the 8 cells each variant won on hook rate
# V1 wins the hook rate in ALL 8 condition x round cells - a completely dominant
# hook clip.

cell_winners %>% count(ctr_winner)
# V1 also wins CTR in 7 of the 8 cells (V3 takes 1) - the same clip advantage
# carries through to clicks.

write_csv(cell_winners, here("output/tables/study2_expl_hookvariant_cellwinners.csv"))


