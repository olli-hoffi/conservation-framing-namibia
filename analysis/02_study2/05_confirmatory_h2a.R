# =============================================================================
# 02_study2/05_confirmatory_h2a.R
#
# WHERE THIS BELONGS
#   In the thesis: Chapter 4, Results: Moderator Analyses
#   Produces: Table 13, the H2a rows
#   Status: PREREGISTERED. Registered with AsPredicted on 10 June 2026, before
#   the campaign launched. Nothing here was decided after seeing the data.
#
# Study 2 - H2a as REGISTERED: % female per ad vs. the EMPATHY-MINUS-NEUTRAL
# CTR contrast (not vs. total CTR).
#
# Why this script exists. The preregistration (AsPredicted, 2026-06-10, quoted
# here in thesis labels) operationalizes H2a as:
#   "Gender = preregistered moderator (aggregated Meta Ads Manager breakdown,
#    % female per ad) ... Spearman rho between moderator and the empathy-vs-
#    neutral contrast (H2a: CTR; H2b: time on page)."
# Section L of 02_study2/14_exploratory.R correlates % female with an empathy ad's
# TOTAL CTR. That is a different quantity: total CTR carries the level of the ad,
# not the empathy-versus-neutral difference the hypothesis is about. This script
# computes the registered contrast version, and recomputes the total-CTR version
# alongside it so the two can be read side by side.
#
# Unit of analysis. The contrast is BETWEEN conditions, so the moderator and the
# outcome have to meet on a shared unit. The design supplies one: per Appendix C
# (and the design note in 02_study2/12_hook_variants.R), the Section A hook footage
# is identical across the four framing conditions of a given variant and differs
# only BETWEEN variants. Variant index is therefore a matching key, not an
# arbitrary label, and each empathy ad has a footage-matched neutral counterpart
# within the same round. That yields
#     3 variants x 2 rounds = 6 matched empathy-neutral pairs.
# The coarser alternative (condition means within round) gives n = 2 and is not
# estimable; it is reported as such rather than computed.
#
# Status of the result. Two things bound it, and both are reported, not resolved:
#   (1) The registered gate "H2a, H2b and H6 are tested inferentially only if
#       total exit-survey N >= 20" was not met (N = 13). The gate is written at
#       the level of the family {H2a, H2b, H6}, with no carve-out for H2a, even
#       though H2a's moderator comes from the Meta breakdown and not from the
#       survey. Read literally, the gate bars an inferential H2a. This script
#       therefore computes the registered statistic as NON-CONFIRMATORY.
#   (2) Independently of the gate, n = 6 pairs and a female-share range of
#       roughly two percentage points leave a Spearman rho with essentially no
#       resolution. The bootstrap CI below is printed to show that, not to
#       license an inference from it.
#
# Inputs (data/raw/MetaAds/): MetaAds_demographics_*.csv  (round x ad x age x gender)
# Outputs (output/tables/):
#   study2_h2a_registered_pairs.csv / .docx     the 6 matched pairs
#   study2_h2a_registered_spearman.csv / .docx  registered vs. total-CTR, side by side
#   study2_h2a_registered_moderator_range.csv   spread of the moderator
#
# Run order: 02_study2/03_process_campaign.R -> 02_study2/04_confirmatory.R -> 02_study2/14_exploratory.R -> this.
# =============================================================================

# pacman::p_load() installs a package if missing, then loads it, in one call.
pacman::p_load(here, tidyverse, flextable, officer)

# Source every helper in src/ (theme_apa(), condition_colours, save_apa(),
# theme_flex_apa(), save_apa_table(), fmt_* formatters). walk() runs source() on
# each file for its side effect and returns nothing.
list.files(here("src"), full.names = TRUE) %>% walk(source)


# ---- Analysis-wide constants ------------------------------------------------

set.seed(42)     # lock the bootstrap resamples so every CI reproduces exactly
n_boot <- 5000   # bootstrap resamples for every percentile CI in this script

# Neutral listed FIRST so it becomes the factor reference level - the control the
# motivational frames are compared against.
cond_levels <- c("Neutral", "Empathy", "Social Norm", "Narrative")


# ---- Spearman rho with percentile bootstrap CI ------------------------------
# Same helper as 02_study2/14_exploratory.R, repeated here so this script runs
# standalone. Spearman (rank correlation, not Pearson) because CTR is a small-n,
# skewed rate: it correlates ranks and assumes no normality or linearity.
# exact_p = TRUE asks cor.test for the exact permutation p, which is available at
# n = 6 provided there are no tied ranks; it falls back to the asymptotic p.
spearman_boot <- function(x, y, reps = n_boot) {
  ok <- complete.cases(x, y)   # keep only pairs where BOTH x and y are present
  x  <- x[ok]; y <- y[ok]      # drop the incomplete pairs from both vectors
  n  <- length(x)              # effective sample size after listwise deletion

  # Guard: a correlation is undefined with < 3 points or if either side is
  # constant (zero variance -> no ranks to correlate).
  if (n < 3 || sd(x) == 0 || sd(y) == 0) {
    return(tibble(n = n, rho = NA_real_, ci_low = NA_real_, ci_high = NA_real_,
                  p = NA_real_, p_type = NA_character_,
                  note = "not estimable (n < 3 or zero variance)"))
  }

  rho <- cor(x, y, method = "spearman")   # point estimate (rank correlation)

  # Ties break the exact algorithm, so pick the estimator the data allow and
  # record WHICH one was used rather than silently mixing them.
  has_ties <- any(duplicated(rank(x))) || any(duplicated(rank(y)))
  ct <- suppressWarnings(
    cor.test(x, y, method = "spearman", exact = !has_ties)
  )
  p_type <- if (has_ties) "asymptotic (tied ranks)" else "exact permutation"

  # Percentile bootstrap: resample the PAIRS with replacement, recompute rho each
  # time; the spread of those rho's stands in for the sampling distribution. At
  # n = 6 many resamples are degenerate (repeated rows -> zero variance -> NA),
  # so the CI is a spread description, not a precision claim.
  boot_r <- replicate(reps, {
    i <- sample(n, replace = TRUE)                          # random indices, with replacement
    suppressWarnings(cor(x[i], y[i], method = "spearman"))  # rho on this resample
  })
  n_valid <- sum(!is.na(boot_r))                            # non-degenerate resamples
  ci <- quantile(boot_r, c(0.025, 0.975), na.rm = TRUE)     # 2.5th / 97.5th pct = 95% CI

  tibble(
    n = n, rho = round(rho, 3),
    ci_low = round(ci[1], 3), ci_high = round(ci[2], 3),
    p = round(as.numeric(ct$p.value), 4),
    p_type = p_type,
    note = paste0("percentile bootstrap, ", n_valid, "/", reps, " valid resamples")
  )
}


# ---- Load the ad-level gender breakdown -------------------------------------
# From pull_extras.py (Marketing API breakdowns): one row per round x ad x age
# band x gender. This is the registered H2a moderator source and exists whether
# or not anyone completed the exit survey.

demo_files <- list.files(here("data/raw/MetaAds"),
                         pattern = "^MetaAds_demographics_", full.names = TRUE)

stopifnot(length(demo_files) > 0)   # without the breakdown H2a is untestable by construction

demo <- demo_files %>%
  map_dfr(~ read_csv(.x, show_col_types = FALSE)) %>%
  mutate(
    # The ad-name slug encodes both the frame and the variant index
    # (e.g. WC_empathy_2 = Empathy, variant 2).
    condition = factor(case_when(
      str_detect(ad_name, "neutral")    ~ "Neutral",
      str_detect(ad_name, "empathy")    ~ "Empathy",
      str_detect(ad_name, "socialnorm") ~ "Social Norm",
      str_detect(ad_name, "narrative")  ~ "Narrative"
    ), levels = cond_levels),
    variant = as.integer(str_extract(ad_name, "\\d+$")),
    round   = factor(round)
  )

# Collapse the age bands away: one row per round x ad x gender. "unknown" and
# other gender codes are dropped so female share is a clean two-way split, the
# same rule Section K/L of 04b uses.
ad_gender <- demo %>%
  filter(gender %in% c("female", "male")) %>%
  group_by(round, condition, variant, ad_name, gender) %>%
  summarise(impressions = sum(impressions),
            link_clicks = sum(link_clicks), .groups = "drop")

# One row per ad-round, with the female/male columns side by side.
ad_wide <- ad_gender %>%
  pivot_wider(names_from = gender, values_from = c(impressions, link_clicks)) %>%
  mutate(
    impressions_total = impressions_female + impressions_male,
    clicks_total      = link_clicks_female + link_clicks_male,
    pct_female        = impressions_female / impressions_total,   # THE registered moderator
    ctr_total         = clicks_total / impressions_total          # the ad's pooled CTR
  )

nrow(ad_wide)
# 24 ad-rounds (12 ads x 2 rounds).


# ---- Moderator spread -------------------------------------------------------
# Reported before the correlation because it caps what any correlation can show:
# a rank correlation over a moderator that barely moves is a ranking of noise.

# Three groupings, because the chapter quotes three different spans and each
# answers a different question. The ad-round rows are the unit the correlation
# runs on. The per-creative pooling is what a reader sees when the two rounds of
# one advertisement are read together. The empathy subset is the six rows that
# actually enter the registered contrast.
ad_pooled <- ad_wide %>%
  group_by(condition, variant, ad_name) %>%
  summarise(pct_female = sum(impressions_female) / sum(impressions_total),
            .groups = "drop")

spread <- function(x) tibble(n = length(x),
                            min_pct_f = round(min(x), 4),
                            max_pct_f = round(max(x), 4),
                            range_pp  = round((max(x) - min(x)) * 100, 2),
                            sd_pp     = round(sd(x) * 100, 2))

mod_range <- bind_rows(
  spread(ad_wide$pct_female)  %>% mutate(unit = "ad-round", .before = 1),
  spread(ad_pooled$pct_female) %>% mutate(unit = "advertisement, rounds pooled", .before = 1),
  spread(ad_wide$pct_female[ad_wide$condition == "Empathy"]) %>%
    mutate(unit = "empathy ad-round", .before = 1)
)

mod_range
# Female share of delivery moves within a narrow band across all 24 ad-rounds
# (see printed values). Meta delivered a near-constant gender mix to every ad, so
# the registered moderator has very little between-ad variance to correlate with.

write_csv(mod_range, here("output/tables/study2_h2a_registered_moderator_range.csv"))


# ---- Build the registered contrast: empathy minus footage-matched neutral ----
# Join each empathy ad to the neutral ad sharing its variant index AND round.
# Section A hook footage is identical within a variant across conditions, so the
# pair differs in framing, not in opening footage.

emp <- ad_wide %>%
  filter(condition == "Empathy") %>%
  dplyr::select(round, variant,
         pct_female_emp   = pct_female,
         ctr_emp          = ctr_total,
         imp_emp          = impressions_total,
         imp_f_emp        = impressions_female,
         clicks_emp       = clicks_total)

neu <- ad_wide %>%
  filter(condition == "Neutral") %>%
  dplyr::select(round, variant,
         pct_female_neu   = pct_female,
         ctr_neu          = ctr_total,
         imp_neu          = impressions_total,
         imp_f_neu        = impressions_female,
         clicks_neu       = clicks_total)

pairs <- emp %>%
  inner_join(neu, by = c("round", "variant")) %>%
  mutate(
    # The registered outcome: the empathy-versus-neutral CTR contrast.
    ctr_diff_pp   = (ctr_emp - ctr_neu) * 100,        # difference, percentage points
    ctr_log_ratio = log(ctr_emp / ctr_neu),           # ratio, log scale (sensitivity)
    # Two readings of "% female per ad" for a PAIR: the empathy ad's own share
    # (the ad carrying the manipulation) and the share pooled over both ads.
    pct_female_pair = (imp_f_emp + imp_f_neu) / (imp_emp + imp_neu)
  ) %>%
  arrange(round, variant)

pairs %>%
  dplyr::select(round, variant, pct_female_emp, pct_female_pair,
         ctr_emp, ctr_neu, ctr_diff_pp, ctr_log_ratio)
# 6 matched pairs. The empathy-minus-neutral contrast changes sign across them,
# which is the first sign that there is no stable empathy advantage to moderate.

write_csv(pairs, here("output/tables/study2_h2a_registered_pairs.csv"))
save_apa_table(
  pairs %>%
    transmute(round, variant,
              pct_female_emp = round(pct_female_emp * 100, 2),
              ctr_emp        = round(ctr_emp * 100, 3),
              ctr_neu        = round(ctr_neu * 100, 3),
              ctr_diff_pp    = round(ctr_diff_pp, 3)),
  "study2_h2a_registered_pairs",
  title = "Table. Empathy-Minus-Neutral CTR Contrast by Footage-Matched Variant and Round",
  note  = paste("Each row pairs an empathy advertisement with the neutral advertisement",
                "sharing its hook-footage variant within the same campaign round.",
                "pct_female_emp = female share of the empathy advertisement's impressions (%).",
                "CTR values in %; ctr_diff_pp = empathy minus neutral in percentage points.",
                "Gender breakdown covers female and male impressions only."),
  digits = 3
)


# ---- The registered test, and the total-CTR version it was confused with -----
# Row 1 is the preregistered analysis. Rows 2-3 are sensitivity readings of the
# same hypothesis. Row 4 is what the thesis reported instead: % female against
# the empathy ad's TOTAL CTR, which contains no neutral comparison at all.

h2a_tests <- bind_rows(
  spearman_boot(pairs$ctr_diff_pp,   pairs$pct_female_emp) %>%
    mutate(specification = "REGISTERED: % female (empathy ad) x empathy-minus-neutral CTR contrast (pp)",
           .before = 1),
  spearman_boot(pairs$ctr_diff_pp,   pairs$pct_female_pair) %>%
    mutate(specification = "Sensitivity: % female (pair-pooled) x empathy-minus-neutral CTR contrast (pp)",
           .before = 1),
  spearman_boot(pairs$ctr_log_ratio, pairs$pct_female_emp) %>%
    mutate(specification = "Sensitivity: % female (empathy ad) x log(empathy CTR / neutral CTR)",
           .before = 1),
  spearman_boot(ad_wide %>% filter(condition == "Empathy") %>% pull(ctr_total),
                ad_wide %>% filter(condition == "Empathy") %>% pull(pct_female)) %>%
    mutate(specification = "AS PREVIOUSLY REPORTED: % female (empathy ad) x TOTAL CTR of that ad (no contrast)",
           .before = 1)
)

h2a_tests
# Read the two flagged rows against each other: the registered contrast version
# and the total-CTR version are different statistics on the same six ad-rounds.

write_csv(h2a_tests, here("output/tables/study2_h2a_registered_spearman.csv"))
save_apa_table(
  h2a_tests %>% dplyr::select(specification, n, rho, ci_low, ci_high, p, p_type),
  "study2_h2a_registered_spearman",
  title = "Table. H2a: Preregistered Empathy-Versus-Neutral Contrast and the Total-CTR Alternative",
  note  = paste("Spearman rho; 95% CI from a percentile bootstrap with 5,000 resamples.",
                "n = 6 in every row (3 hook-footage variants x 2 campaign rounds), so all",
                "estimates are unstable and are reported descriptively, not as tests.",
                "The exit-survey gate of N >= 20 for the family {H2a, H2b, H6} was not met",
                "(N = 13), so no row is confirmatory."),
  digits = 3
)


# ---- Coarser pairing, for the record [not reported] --------------------------
# Condition means within round is the other level at which the contrast could be
# formed. It gives one empathy-minus-neutral value per round, so n = 2: below the
# minimum for any correlation. Computed and shown as descriptive values only.

round_level <- ad_wide %>%
  filter(condition %in% c("Empathy", "Neutral")) %>%
  group_by(round, condition) %>%
  summarise(impressions = sum(impressions_total),
            impressions_female = sum(impressions_female),
            clicks = sum(clicks_total), .groups = "drop") %>%
  mutate(ctr = clicks / impressions, pct_female = impressions_female / impressions) %>%
  dplyr::select(round, condition, pct_female, ctr) %>%
  pivot_wider(names_from = condition, values_from = c(pct_female, ctr)) %>%
  mutate(ctr_diff_pp = round((ctr_Empathy - ctr_Neutral) * 100, 4))

round_level
# With n = 2 rounds a correlation is undefined, so none is computed.
# -> two points are too few for a correlation to exist, not merely too few to
#    trust. H2a is reported from the six matched variant-by-round pairs in Table 13.


# ---- Console summary --------------------------------------------------------

cat("\n--- H2a, registered operationalization ---\n")
cat("Pairs (variant x round):", nrow(pairs), "\n")
cat("Moderator range (pct female, pp):", mod_range$range_pp, "\n\n")
print(as.data.frame(h2a_tests))
cat("\nGate: exit-survey N = 13 < 20. Family {H2a, H2b, H6} gate not met.\n")
cat("All rows above are descriptive / non-confirmatory.\n")
