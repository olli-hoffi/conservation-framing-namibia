# =============================================================================
# 02_study2/09_engagement_models.R
#
# WHERE THIS BELONGS
#   In the thesis: Chapter 4, Results: Exploratory Analyses
#   Produces: Figure F1, Tables F3 and F4, and the CTR column of Table 16
#   (link_ctr in study2_expl_engagement_rates.csv; the other Table 16 rows
#   come from 14_exploratory.R)
#   Status: Exploratory. Study 2 has a preregistration, but this script is not
#   part of it. Only 04_confirmatory.R and 05_confirmatory_h2a.R are, and any
#   result here is a finding to be retested rather than a test.
#
# Study 2 - EXPLORATORY engagement models beyond the click.
# The confirmatory CTR hypotheses (H1, H3, H4) concern CTR only. This script extends the
# same modelling logic to other behavioural outcomes Meta delivers, to ask whether
# framing moved engagement that is NOT a link click. Everything here is
# NON-preregistered, reported two-tailed without alpha correction, effect sizes
# (OR) with 95% CIs prioritised.
#
# Outcomes modelled as rate = action / impressions, quasi-binomial GLM vs. Neutral:
#   - reactions   (~1,100 per condition: solid N)
#   - shares      (54-81 per condition: small, interpret cautiously)
#   - any_action  (reactions + comments + shares + saves: composite, solid N)
# Saves (~18) and comments (~15) are too sparse to model alone; they enter the
# composite and are reported descriptively. Quasi-binomial estimation is used
# (robust to the ad-level overdispersion seen for CTR); dispersion phi is reported.
#
# Also: delivery-to-engagement FUNNEL per condition
#   impressions -> 3s video views -> link clicks -> GA4 sessions -> engaged sessions
#
# Inputs (data/processed/): meta_ads_clean.csv, ga4_clean.csv
# Outputs (output/tables/): study2_expl_engagement_glm.csv,
#   study2_expl_engagement_rates.csv, study2_expl_engagement_funnel.csv
# Figures (output/figures/): fig_study2_hook_ctr_scatter
#
# Run order: 01b -> 04 -> 04b -> 04c -> 04d.
# =============================================================================

# pacman::p_load() installs a package if missing, then loads it, in one call.
pacman::p_load(here, tidyverse, broom, scales, ggrepel, flextable, officer)

# Source every helper in src/ (theme_apa(), condition_colours, save_apa(),
# save_apa_table(), apa_width_full). walk() runs source() on each file for its
# side effect and returns nothing.
list.files(here("src"), full.names = TRUE) %>% walk(source)


# ---- Constants + data -------------------------------------------------------

# Four framing conditions in FIXED display order (Neutral first = the reference the
# motivational frames are compared against).
cond_levels  <- c("Neutral", "Empathy", "Social Norm", "Narrative")
# The three motivational frames only (everything that is not the control).
motivational <- c("Empathy", "Social Norm", "Narrative")

# Cleaned per-ad Meta export (one row per ad = condition x variant x round).
meta <- here("data/processed/meta_ads_clean.csv") %>%
  read_csv(show_col_types = FALSE) %>%
  mutate(
    # fct_relevel() sets "Neutral" as reference so every GLM coefficient reads
    # "frame vs. Neutral".
    condition  = fct_relevel(factor(condition, levels = cond_levels), "Neutral"),
    round      = factor(round),                           # round is a grouping label, not a quantity
    any_action = reactions + comments + shares + saves    # composite: every non-click interaction
  )


# ---- S. Engagement-rate GLMs (reactions / shares / any action) --------------
# One quasi-binomial GLM per outcome per round: outcome / impressions ~ condition.

# Fits the same model for whichever engagement column you name.
#   success_col = the count column (e.g. "reactions"); label = pretty name for the table.
engagement_glm <- function(success_col, label) {
  # map_dfr(): loop over each round separately, then row-bind the per-round tables.
  map_dfr(levels(meta$round), function(r) {
    d <- filter(meta, round == r)                          # subset to this round's ads
    d$condition <- relevel(d$condition, ref = "Neutral")   # re-assert Neutral as reference within the subset
    # cbind(successes, failures) response: successes = the action count, failures =
    # impressions that did NOT produce the action. Modelling action/impressions this
    # way lets the GLM weight each ad by its impression volume. quasibinomial =
    # binomial that ESTIMATES a dispersion parameter instead of fixing it at 1,
    # widening SEs under overdispersion. logit link = log-odds scale.
    m <- glm(cbind(d[[success_col]], d$impressions - d[[success_col]]) ~ condition,
             data = d, family = quasibinomial(link = "logit"))
    disp <- summary(m)$dispersion                          # phi: 1 = none, > 1 = overdispersed
    # Interval and test must answer to the SAME reference distribution. A quasi
    # model is judged on t with df.residual degrees of freedom, so the CI is a Wald
    # interval on that same t, NOT broom's conf.int = TRUE (profile likelihood,
    # which references the normal and is therefore too narrow beside the p value
    # reported next to it). 02_study2/04_confirmatory.R uses the same Wald-on-t
    # interval, and Table 12's note records it.
    t_crit <- qt(0.975, df.residual(m))
    # tidy(exponentiate = FALSE) keeps the log-odds scale for the interval
    # arithmetic; exp() below returns ODDS RATIOS (OR > 1 = more engagement).
    tidy(m, exponentiate = FALSE) %>%
      filter(str_detect(term, "condition")) %>%            # drop intercept; keep the 3 frame-vs-Neutral rows
      transmute(                                           # transmute = mutate + keep only these columns
        outcome    = label,
        round      = r,
        condition  = str_remove(term, "condition"),        # "conditionEmpathy" -> "Empathy"
        OR         = round(exp(estimate), 3),              # odds ratio vs. Neutral
        ci_lower   = round(exp(estimate - t_crit * std.error), 3),
        ci_upper   = round(exp(estimate + t_crit * std.error), 3),
        p_two      = round(p.value, 4),                    # two-tailed (exploratory: no directional claim)
        dispersion = round(disp, 2)
      )
  })
}

# Run the helper for each modelled outcome and stack the three tables together.
eng_results <- bind_rows(
  engagement_glm("reactions",  "Reactions"),
  engagement_glm("shares",     "Shares"),
  engagement_glm("any_action", "Any interaction")
)

eng_results
# No frame reliably beats Neutral on any engagement outcome: reaction ORs sit at
# 0.85-1.06, share ORs 0.60-1.45 (wide CIs, all spanning 1), any-interaction ORs
# 0.85-1.03. The only nominally "significant" cells are Social Norm DOWN in Round 2
# (reactions OR = 0.85, p = .040; any interaction OR = 0.85, p = .030) - negative,
# uncorrected, and not replicated in R1. Overall: framing did not move non-click
# engagement. Dispersion phi 0.71-1.79 (R1 more overdispersed than R2).

write_csv(eng_results, here("output/tables/study2_expl_engagement_glm.csv"))
save_apa_table(eng_results, "study2_expl_engagement_glm",
               title = "Table. Engagement-Rate GLMs vs. Neutral (Quasi-Binomial, Two-Tailed, Exploratory)",
               note  = "OR = odds ratio vs. Neutral. dispersion = phi (1 = no overdispersion). p_two = two-tailed.",
               digits = 3)

# Descriptive rates per condition (pooled across rounds): plain observed rates
# (count / impressions) summed over both rounds, as the raw picture beside the model.
eng_rates <- meta %>%
  group_by(condition) %>%
  summarise(
    impressions   = sum(impressions),
    reaction_rate = round(sum(reactions) / sum(impressions), 5),   # reactions per impression
    share_rate    = round(sum(shares) / sum(impressions), 6),      # shares are rarer -> more decimals
    save_rate     = round(sum(saves) / sum(impressions), 6),
    comment_rate  = round(sum(comments) / sum(impressions), 6),
    any_rate      = round(sum(any_action) / sum(impressions), 5),  # composite rate
    link_ctr      = round(sum(link_clicks) / sum(impressions), 5), # the confirmatory DV, for reference
    .groups = "drop"
  )

eng_rates
# Raw rates confirm the flat GLM picture: reaction rate ~1.0-1.1% and any-interaction
# rate ~1.1-1.2% in every condition; Social Norm is marginally lowest (reaction .99%,
# any 1.08%). Shares/saves/comments are rare (< 0.08%). Link CTR is the DV for
# reference: .41% Neutral -> .48% Narrative (the familiar Narrative edge).

write_csv(eng_rates, here("output/tables/study2_expl_engagement_rates.csv"))


# ---- T. Delivery-to-engagement funnel ---------------------------------------
# Stages: impressions -> 3s views -> link clicks -> GA4 sessions -> engaged sessions
# (sessions x engagement_rate). GA4 counts are tiny and consent-gated, so the last
# two stages are indicative only.

# GA4 = Google Analytics 4 (the landing-page side of the funnel).
ga4 <- here("data/processed/ga4_clean.csv") %>%
  read_csv(show_col_types = FALSE) %>%
  mutate(condition = factor(condition, levels = cond_levels))

# Collapse GA4 to one row per condition. ORDERING TRICK: engaged_sessions is computed
# from the per-row `sessions` BEFORE the next line overwrites `sessions` with the
# group total (summarise evaluates its expressions sequentially).
ga4_by_cond <- ga4 %>%
  group_by(condition) %>%
  summarise(engaged_sessions = round(sum(sessions * engagement_rate)),  # sessions weighted by their engagement rate
            sessions = sum(sessions),
            .groups = "drop")

# Build the funnel: Meta stages (impressions -> 3s views -> link clicks) joined to
# the GA4 stages (sessions -> engaged sessions).
funnel <- meta %>%
  group_by(condition) %>%
  summarise(impressions = sum(impressions),
            views_3s = sum(video_3s_views),
            link_clicks = sum(link_clicks), .groups = "drop") %>%
  left_join(ga4_by_cond, by = "condition") %>%             # attach the GA4 counts by condition
  mutate(
    # Stage-to-stage conversion ratios (each = this stage / the previous stage):
    hook_of_impr   = round(views_3s / impressions, 4),     # did the hook stop the scroll? (3s view rate)
    click_of_3s    = round(link_clicks / views_3s, 4),     # of 3s-watchers, who clicked the link?
    session_of_clk = round(sessions / link_clicks, 4),     # of clickers, who actually landed (GA4 session)?
    engaged_of_ses = round(engaged_sessions / sessions, 4) # of landers, who engaged on the page?
  )

funnel
# The funnel narrows the same way in every condition: hook rate (3-s view / impr)
# ~19% across the board, click-of-3s ~2.2-2.6% (Narrative highest at 2.6%),
# session-of-click ~3-4%, engaged-of-session .47-.65%. GA4 counts are tiny (13-19
# sessions, 7-11 engaged per condition) so the last two stages are indicative only.
# Narrative's CTR edge comes from converting 3-s viewers slightly better, not a
# bigger hook.

write_csv(funnel, here("output/tables/study2_expl_engagement_funnel.csv"))
save_apa_table(funnel, "study2_expl_engagement_funnel",
               title = "Table. Delivery-to-Engagement Funnel by Framing Condition (Exploratory)",
               note  = "Stage ratios = stage / previous stage. Session stages from consent-gated GA4 (indicative).",
               digits = 4)


# ---- Figures ----------------------------------------------------------------

# Figure: OR forest plot - CTR + engagement outcomes, pooled by refitting on both
# rounds together for one readable panel.
# Fit ONE pooled quasi-binomial GLM and return just the OR + CI for the three
# frame-vs-Neutral contrasts.
or_pool <- function(success_col, label) {
  m <- glm(cbind(meta[[success_col]], meta$impressions - meta[[success_col]]) ~ condition,
           data = meta, family = quasibinomial(link = "logit"))
  tidy(m, exponentiate = TRUE, conf.int = TRUE) %>%
    filter(str_detect(term, "condition")) %>%
    transmute(outcome = label, condition = str_remove(term, "condition"),
              OR = estimate, lo = conf.low, hi = conf.high)
}

# Stack the four outcomes (the confirmatory CTR plus the three exploratory ones).
forest <- bind_rows(
  or_pool("link_clicks", "Link CTR"),
  or_pool("reactions",   "Reactions"),
  or_pool("shares",      "Shares"),
  or_pool("any_action",  "Any interaction")
) %>%
  # Fix the facet + colour ordering so the plot reads top-to-bottom consistently.
  mutate(outcome   = factor(outcome, levels = c("Link CTR", "Reactions",
                                                "Shares", "Any interaction")),
         condition = factor(condition, levels = motivational))

ad_hook <- meta %>%
  group_by(condition, variant) %>%
  summarise(hook_rate = sum(video_3s_views) / sum(impressions),   # x: how well the ad stopped the scroll
            link_ctr  = sum(link_clicks) / sum(impressions),      # y: how well it converted to a click
            .groups = "drop")

# No fitted line. The hook rate is bimodal: nine ads sit between 15 and 17.5%
# and three between 24 and 25%, with nothing in between, so a linear fit would
# be drawn across an empty gap and its slope set by the gap rather than by
# within-cluster variation. The rho is reported in the subtitle instead, and the
# two-group structure is what the figure is for.
p_hook <- ad_hook %>%
  ggplot(aes(hook_rate * 100, link_ctr * 100)) +                  # *100 -> express both as percentages
  geom_point(aes(colour = condition, shape = condition), size = 2.5) +  # each dot = one of the 12 ads
  scale_colour_manual(values = condition_colours, name = NULL) +
  # Shape repeats the condition so the four series do not rely on hue alone (APA 7.26).
  scale_shape_manual(values = c(16, 17, 15, 18), name = NULL) +
  labs(title = "Hook Rate Predicts Click-Through at the Ad Level",
       # No statistics in the subtitle. theme_apa() blanks title and subtitle (APA 7
       # puts the figure number, title and note in the text), so a value written here
       # never reaches a reader and silently goes stale. rho and its interval are
       # computed in 14_exploratory.R and reported in the figure note of the thesis.
       subtitle = NULL,
       x = "Hook Rate: 3-Second Views per Impression (%)",
       y = "Link CTR (%)") +
  theme_apa(legend_pos = "bottom")

p_hook
save_apa(p_hook, "fig_study2_hook_ctr_scatter", width = apa_width_full, height = 4.5)
