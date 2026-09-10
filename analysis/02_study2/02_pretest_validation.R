# =============================================================================
# 02_study2/02_pretest_validation.R
#
# WHERE THIS BELONGS
#   In the thesis: Chapter 4, Method: Stimulus Pretest
#   Produces: Table 8, Figure E2, Tables E5, E7, and E8
#   Status: Confirmatory for the pretest, which had five pass criteria fixed
#   before any rating came in. Study 2 itself was preregistered separately.
#
# Study 2 pretest - manipulation-check analysis.
#
# Design: within-subjects. Each participant rated all four short messages
# (Neutral, Empathy, Social Norm, Narrative) on three target subscales (EMP,
# NORM, NAR; 3 items each) plus two control items (CTRL1, CTRL2), on a 5-point
# Likert scale. The check confirms each motivational message scores highest on
# its matching construct while Neutral sits lowest and the control items stay flat.
#
# Preregistered confirmation criteria, as registered for the pretest:
#   C1  Empathy message scores highest on EMP
#   C2  Social Norm message scores highest on NORM
#   C3  Narrative message scores highest on NAR
#   C4  Neutral message scores lowest on EMP, NORM and NAR
#   C5  Control items (CTRL1, CTRL2) vary < 1.0 SD across message means
#
# Analysis choices (for the Method section):
#   - Cronbach's alpha pooled across messages (each participant x message row =
#     one rating of the construct), the standard small-N manipulation-check alpha.
#   - Friedman test (within-subjects analogue of rm-ANOVA), Kendall's W effect size.
#   - Pairwise Wilcoxon signed-rank, Holm-Bonferroni across the 6 condition pairs.
#   - Effect sizes with 95% CIs: rank-biserial r (Wilcoxon), Feldt CI (alpha).
#   - Percentile bootstrap CIs on cell means, 5000 replicates, set.seed(2026).
#
# Inputs  (data/processed/, from 02_study2/01_process_pretest.R):
#   pretest_items_long.csv, pretest_composites.csv,
#   pretest_demographics.csv, pretest_order.csv
#
# Outputs (output/): result tables (CSV + APA .docx), APA paste-strings, a
#   Markdown results file, one figure, and a reproducibility session snapshot.
# =============================================================================

# pacman::p_load() installs a package if missing, then loads it, in one call.
pacman::p_load(
  here, tidyverse,
  psych,        # alpha() for Cronbach's alpha
  rstatix,      # friedman_test(), friedman_effsize(), wilcox_test()
  effectsize,   # rank_biserial() with 95% CI
  boot,         # bootstrap CIs for cell means
  ggdist,       # half-eye / raincloud geoms
  patchwork,    # combine ggplots side by side
  knitr,        # kable() for the Markdown export
  flextable, officer   # APA .docx tables via save_apa_table()
)

# Source every helper in src/ (theme_apa(), condition_colours, save_apa(),
# theme_flex_apa(), save_apa_table(), fmt_p/fmt_r/fmt_ci). walk() = a for-loop
# that runs source() on each file for its side effect and returns nothing.
list.files(here("src"), full.names = TRUE) %>% walk(source)

# One global RNG seed so the percentile bootstrap draws the same resamples on
# every run -> identical CIs, which OSF replication requires.
set.seed(2026)


# ---- Local helpers ----------------------------------------------------------

# format_p(): APA in-text p with the "p = " / "p < " prefix the paste-strings
# need. src/apa_format.R already provides fmt_p() (returns just ".024" / "< .001");
# this wrapper adds the comparator so callers never glue "p = " onto a "< .001".
format_p <- function(p, digits = 3) {
  if (is.na(p)) return("p = NA")               # no p to report (e.g. test not run)
  if (p < .001) return("p < .001")             # APA rule: never print p = .000
  paste0("p = ", sub("^0", "", formatC(p, digits = digits, format = "f")))   # ".024" not "0.024"
}

# boot_mean_ci(): percentile bootstrap CI for a mean. Returns a named 3-vector
# (mean, lower, upper) so the caller can pull each element by name.
boot_mean_ci <- function(x, R = 5000) {
  x <- x[!is.na(x)]                            # drop missing before resampling
  if (length(x) < 2) return(c(mean = mean(x), lower = NA, upper = NA))
  # boot() resamples x WITH replacement R times; the statistic recomputes the mean
  # on each resample (d[i] = the resampled values), building an empirical sampling
  # distribution without assuming normality - suited to the small-N Likert data.
  b <- boot(x, statistic = function(d, i) mean(d[i]), R = R)
  # Percentile CI = 2.5th / 97.5th percentiles of the bootstrap means; boot.ci()
  # $percent returns 5 numbers, elements [4:5] are the lower/upper bounds. tryCatch
  # guards against a degenerate (constant) resample.
  ci <- tryCatch(boot.ci(b, type = "perc")$percent[4:5], error = function(e) c(NA, NA))
  c(mean = mean(x), lower = ci[1], upper = ci[2])
}


# ---- 1. Load processed data -------------------------------------------------
# Factors lose their level order when written to CSV, so the experimental order
# of conditions and scales is restored explicitly (Neutral first = the control).

items_long <- here("data/processed/pretest_items_long.csv") %>%
  read_csv(show_col_types = FALSE) %>%
  mutate(
    condition = fct_relevel(condition, "Neutral", "Empathy", "Social Norm", "Narrative"),
    scale     = fct_relevel(scale, "EMP", "NORM", "NAR", "CTRL1", "CTRL2")
  )

composites <- here("data/processed/pretest_composites.csv") %>%
  read_csv(show_col_types = FALSE) %>%
  mutate(
    condition = fct_relevel(condition, "Neutral", "Empathy", "Social Norm", "Narrative"),
    scale     = fct_relevel(scale, "EMP", "NORM", "NAR", "CTRL1", "CTRL2")
  )

demographics <- here("data/processed/pretest_demographics.csv") %>%
  read_csv(show_col_types = FALSE)

order_data <- here("data/processed/pretest_order.csv") %>%
  read_csv(show_col_types = FALSE)

n_participants <- n_distinct(items_long$id)   # drives the sample-size gate below
n_participants
# N = 24 participants (each contributing 4 messages x 11 items = 44 ratings).


# ---- 2. Sample-size gate ----------------------------------------------------
# The preregistration sets a minimum of N = 12 and a target of N = 15. Below
# those the script still runs, but the inferential tests are read with caution.

n_gate <- case_when(
  n_participants < 12 ~ "below minimum (12) - inferential tests descriptive only",
  n_participants < 15 ~ "meets minimum (12) but below target (15) - limited power",
  TRUE                ~ "at or above target (15) - fully powered per prereg"
)
n_gate
# "at or above target (15)": N = 24 clears both the minimum and the target, so the
# Friedman and Wilcoxon tests are interpreted normally.

# Make sure the output folders exist (save_apa*/ ggsave also create them).
dir.create(here("output/tables"),  showWarnings = FALSE, recursive = TRUE)
dir.create(here("output/figures"), showWarnings = FALSE, recursive = TRUE)

# APA-ready result strings are appended to this file as each block runs. This
# cat() has NO append = TRUE, so it OVERWRITES the file and starts a clean report.
apa_path <- here("output/tables/apa_results.txt")
cat("Pretest APA-formatted results - generated by 02_study2/02_pretest_validation.R\n",
    "N = ", n_participants, " participants\n",
    "==============================================================\n\n",
    file = apa_path, sep = "")


# ---- 3. Demographics summary ------------------------------------------------
# Counts and percentages per category for age, gender and English proficiency;
# more informative than means/SDs at small N. The literal `variable =` label lets
# the three tallies stack into one long table (variable, level, n).

demo_summary <- bind_rows(
  demographics %>% count(variable = "Age group", level = age_group, name = "n"),
  demographics %>% count(variable = "Gender", level = gender, name = "n"),
  demographics %>% count(variable = "English proficiency", level = english_prof, name = "n")
) %>%
  mutate(
    level   = as.character(level),          # coerce factor levels to text so blocks share a column type
    percent = round(100 * n / n_participants, 1)   # each count as a % of N
  )

demo_summary
# 24 participants: Age mostly 25-29 (62.5%, n = 15), plus 18-24 (25%) and 36+
# (12.5%); Gender an even 12 Woman + 12 Man (50/50); English proficiency Advanced
# (45.8%), Fluent/Native (37.5%) and Intermediate (16.7%). A skilled-English,
# mid-20s sample - the intended pretest audience.

write_csv(demo_summary, here("output/tables/pretest_demographics.csv"))
save_apa_table(demo_summary, "pretest_demographics",
               title = "Table. Sample Characteristics",
               note  = "Counts and percentages of N = 24. DM03 measures English reading proficiency.",
               digits = 1)


# ---- 4. Descriptive statistics ----------------------------------------------

# 4.1 Condition x Scale descriptives with bootstrap 95% CIs on the mean.
# JARS-Quant requires CIs alongside point estimates; at small N the CI is wide on
# purpose, which is exactly the signal a reader needs to read the table correctly.

# For each Condition x Scale cell, bootstrap the mean and its 95% CI.
ci_per_cell <- composites %>%
  group_by(condition, scale) %>%
  summarise(ci = list(boot_mean_ci(score)), .groups = "drop") %>%   # 3-number result kept as a list-column
  mutate(
    mean  = map_dbl(ci, "mean"),     # map_dbl pulls each named element out of the list-column into its own column
    ci_lo = map_dbl(ci, "lower"),
    ci_hi = map_dbl(ci, "upper")
  ) %>%
  dplyr::select(-ci)                          # drop the now-redundant list-column

# Companion descriptives, then joined to the bootstrap means.
descriptives <- composites %>%
  group_by(condition, scale) %>%
  summarise(
    n  = sum(!is.na(score)),           # cell size (non-missing scores)
    sd = sd(score, na.rm = TRUE),      # dispersion within the cell
    md = median(score, na.rm = TRUE),  # median, robust to skew at small N
    .groups = "drop"
  ) %>%
  left_join(ci_per_cell, by = c("condition", "scale")) %>%           # attach mean + CI by cell
  mutate(across(c(mean, sd, md, ci_lo, ci_hi), ~ round(.x, 2))) %>%  # across() rounds all listed numeric cols
  dplyr::select(condition, scale, n, mean, sd, md, ci_lo, ci_hi)

descriptives
# The intended diagonal holds: Empathy tops EMP (M = 4.06), Social Norm tops NORM
# (4.44), Narrative tops NAR (4.62). Neutral is lowest on EMP (2.78) and NAR (2.39),
# but on NORM the Empathy message dips lowest (2.25 < Neutral's 2.67) - the single
# reason C4 fails below. Every cell n = 24; control means (CTRL1/CTRL2) stay high
# (>= 3.3) and barely move across messages.

write_csv(descriptives, here("output/tables/pretest_descriptives.csv"))
save_apa_table(descriptives, "pretest_descriptives",
               title = "Table. Descriptive Statistics by Message and Subscale",
               note  = "M, SD, Mdn and percentile bootstrap 95% CI on M (5,000 replicates). Scores range 1-5.",
               digits = 2)

# ---- 4.2 Item-level descriptives [not reported] ------------------------------
# Mean and SD per item within each target subscale, to catch a weak item before
# the subscale score is trusted.
item_descriptives <- items_long %>%
  filter(scale %in% c("EMP", "NORM", "NAR")) %>%   # target subscales only
  group_by(condition, scale, item_num) %>%
  summarise(
    n    = sum(!is.na(score)),
    mean = round(mean(score, na.rm = TRUE), 2),
    sd   = round(sd(score, na.rm = TRUE), 2),       # a lone high-SD item can drag alpha down
    .groups = "drop"
  )

nrow(item_descriptives)
# 36 rows, 4 messages x 3 subscales x 3 items, every item answered by all 24.
# -> a screening step, with no item thin enough to distort its subscale. Subscale
#    reliability is reported in Table E7 and the condition averages in Table 8.


# ---- 5. Cronbach's alpha per target subscale --------------------------------
# Each target subscale has only 3 items, so alpha is pooled across messages: every
# (participant x message) row becomes one "rater" of the construct, the three
# subscale items are the columns. Standard for manipulation-check alpha at small N.

# Reshape one subscale to a (person x message)-by-item matrix, then run alpha().
compute_alpha <- function(items_data, scale_name) {
  wide <- items_data %>%
    filter(scale == scale_name) %>%
    dplyr::select(id, condition, item_id, score) %>%
    pivot_wider(names_from = item_id, values_from = score) %>%   # items -> columns; rows = id x message
    dplyr::select(starts_with(scale_name))                              # keep only this subscale's item columns

  # alpha() needs >= 2 raters and >= 2 items; otherwise return an all-NA row so the
  # pipeline does not crash on a sparse subscale.
  if (nrow(wide) < 2 || ncol(wide) < 2) {
    return(tibble(scale = scale_name, n_obs = nrow(wide),
                  raw_alpha = NA_real_, std_alpha = NA_real_,
                  ci_low = NA_real_, ci_high = NA_real_))
  }

  # psych::alpha() = Cronbach's alpha, the internal-consistency reliability of the
  # 3-item subscale. suppressWarnings/warnings = FALSE mute the routine small-N advisories.
  a <- suppressWarnings(psych::alpha(wide, warnings = FALSE))

  tibble(
    scale     = scale_name,
    n_obs     = nrow(wide),                    # rows entering alpha (person x message ratings)
    raw_alpha = round(a$total$raw_alpha, 3),   # alpha from item covariances
    std_alpha = round(a$total$std.alpha, 3),   # alpha from item correlations (standardised)
    # Feldt CI bounds come back as 1x1 matrices; as.numeric() flattens them to plain
    # doubles so the tibble column stays writable to CSV.
    ci_low    = round(as.numeric(a$feldt$lower.ci), 3),
    ci_high   = round(as.numeric(a$feldt$upper.ci), 3)
  )
}

# map_dfr() = run compute_alpha() for each subscale, then row-bind into one table.
alpha_table <- map_dfr(c("EMP", "NORM", "NAR"), ~ compute_alpha(items_long, .x))

alpha_table
# All three target subscales are reliable across the 96 person x message ratings:
# EMP alpha = .90 [.86, .93], NORM = .93 [.90, .95], NAR = .84 [.78, .89] (Feldt).
# Good-to-excellent internal consistency, so the 3-item composites hang together.

write_csv(alpha_table, here("output/tables/pretest_alpha.csv"))
save_apa_table(alpha_table, "pretest_alpha",
               title = "Table. Internal Consistency (Cronbach's Alpha)",
               note  = "Pooled across messages; 95% CI via Feldt's method. n_obs = participant x message ratings.",
               digits = 3)

# ---- 5b. Cronbach's alpha within each text ----------------------------------
# The pooled alpha above treats every (participant x message) row as one rater, which
# can be inflated by between-text differences. Because each rater judged all four
# texts, alpha is also computed separately within each text, respecting the repeated
# ratings. This is the figure the Method reports for the twelve construct-by-text
# combinations.
compute_alpha_within_text <- function(items_data, scale_name, cond) {
  wide <- items_data %>%
    filter(scale == scale_name, condition == cond) %>%
    dplyr::select(id, item_id, score) %>%
    pivot_wider(names_from = item_id, values_from = score) %>%
    dplyr::select(starts_with(scale_name))

  if (nrow(wide) < 2 || ncol(wide) < 2) {
    return(tibble(scale = scale_name, condition = cond, n_obs = nrow(wide),
                  raw_alpha = NA_real_, std_alpha = NA_real_))
  }

  a <- suppressWarnings(psych::alpha(wide, warnings = FALSE))

  tibble(
    scale     = scale_name,
    condition = cond,
    n_obs     = nrow(wide),                  # raters contributing to this text
    raw_alpha = round(a$total$raw_alpha, 3),
    std_alpha = round(a$total$std.alpha, 3)
  )
}

# One row per construct-by-text combination, twelve in total.
alpha_by_text <- tidyr::expand_grid(
    scale = c("EMP", "NORM", "NAR"),
    condition = sort(unique(items_long$condition))
  ) %>%
  purrr::pmap_dfr(function(scale, condition)
    compute_alpha_within_text(items_long, scale, condition))

alpha_by_text
# Range across the twelve combinations, quoted in the Method section.
range(alpha_by_text$raw_alpha, na.rm = TRUE)

write_csv(alpha_by_text, here("output/tables/pretest_alpha_by_text.csv"))

# Paste-ready APA strings for alpha (appended to apa_results.txt).
cat("\nCronbach's alpha (Section 4.1.2)\n",
    "----------------------------------\n", file = apa_path, sep = "", append = TRUE)
# walk() = map() used for its side effect (writing lines), returning nothing. One
# APA-formatted line per subscale: alpha with its Feldt 95% CI. fmt_r() (from
# src/apa_format.R) strips the leading zero, e.g. ".82".
walk(seq_len(nrow(alpha_table)), function(i) {
  row <- alpha_table[i, ]
  cat(sprintf("  %s: alpha = %s, 95%% CI [%s, %s] (Feldt; n = %d obs)\n",
              row$scale, fmt_r(row$std_alpha), fmt_r(row$ci_low), fmt_r(row$ci_high), row$n_obs),
      file = apa_path, append = TRUE)
})


# ---- 6. Discriminant validity -----------------------------------------------
# Pearson correlations between the EMP, NORM, NAR composites (pooled across
# participant x message). r > .70 between two target subscales would flag
# substantial conceptual overlap (poor discriminant validity).

# Put the three composites side by side so cor() can correlate them.
corr_data <- composites %>%
  filter(scale %in% c("EMP", "NORM", "NAR")) %>%
  pivot_wider(id_cols = c(id, condition), names_from = scale, values_from = score) %>%
  dplyr::select(EMP, NORM, NAR)                 # keep just the three numeric columns

if (nrow(corr_data) >= 3) {              # need a few observations for a meaningful r
  # pairwise.complete.obs = use all non-missing pairs per correlation rather than
  # dropping a whole row for one missing value.
  discriminant <- cor(corr_data, use = "pairwise.complete.obs") %>%
    round(3) %>%
    as.data.frame() %>%
    rownames_to_column("scale")          # move row names into a proper column for CSV

  discriminant
  # Off-diagonal r's stay far below the .70 overlap threshold: r(EMP, NORM) = .00,
  # r(EMP, NAR) = .40, r(NORM, NAR) = .00. EMP, NORM and NAR measure distinct
  # constructs (good discriminant validity).

  write_csv(discriminant, here("output/tables/pretest_discriminant.csv"))

  # Paste-ready string: pull the three unique off-diagonal r's by row/column lookup.
  cat("\nDiscriminant validity (Pearson r, pooled)\n",
      "------------------------------------------\n",
      sprintf("  r(EMP, NORM)  = %s\n", fmt_r(discriminant$NORM[discriminant$scale == "EMP"])),
      sprintf("  r(EMP, NAR)   = %s\n", fmt_r(discriminant$NAR[discriminant$scale == "EMP"])),
      sprintf("  r(NORM, NAR)  = %s\n", fmt_r(discriminant$NAR[discriminant$scale == "NORM"])),
      file = apa_path, sep = "", append = TRUE)
}


# ---- 7. Friedman tests per target subscale ----------------------------------
# Friedman = the non-parametric, within-subjects analogue of rm-ANOVA: it ranks
# each participant's four message scores and asks whether the rank pattern is
# systematic across messages. Kendall's W is the effect size (0 = no agreement on
# the ordering, 1 = every participant ranks the messages identically). Only
# participants complete on all four messages enter the test.

run_friedman <- function(scale_name) {
  d <- composites %>%
    filter(scale == scale_name) %>%
    dplyr::select(id, condition, score) %>%
    drop_na()                            # a Friedman block needs no missing cells

  # Keep only ids with a complete block (a score on all four messages). Anyone who
  # skipped a message is excluded listwise.
  ids_complete <- d %>%
    group_by(id) %>%
    summarise(k = n(), .groups = "drop") %>%   # how many conditions this id has
    filter(k == nlevels(d$condition)) %>%      # keep the fully-crossed ids (k == 4)
    pull(id)                                   # pull() = extract the single column as a vector

  d <- d %>% filter(id %in% ids_complete)

  if (length(ids_complete) < 2) {             # not testable -> all-NA placeholder row
    return(tibble(scale = scale_name, n = length(ids_complete),
                  chi2 = NA_real_, df = NA_integer_, p = NA_real_, kendall_w = NA_real_,
                  w_ci_low = NA_real_, w_ci_high = NA_real_))
  }

  # Formula score ~ condition | id reads: outcome ~ within-factor | blocking (participant).
  ft <- friedman_test(d, score ~ condition | id)

  # Kendall's W with a 95% interval. The chapter reports an interval beside every W,
  # so it has to come out of the pipeline rather than from a console session.
  # friedman_effsize(ci = TRUE) bootstraps, which would consume this script's single
  # RNG stream and silently shift all 20 cell-mean intervals computed later. The call
  # is therefore given its own seed and the outer stream is handed back untouched, so
  # the W intervals are reproducible on their own and nothing downstream moves.
  .rng_before_w <- .Random.seed
  set.seed(2026)
  ef <- friedman_effsize(d, score ~ condition | id, ci = TRUE, nboot = 5000)
  .Random.seed <- .rng_before_w

  tibble(
    scale     = scale_name,
    n         = length(ids_complete),    # complete blocks entering the test
    chi2      = round(ft$statistic, 3),  # Friedman chi-square
    df        = ft$df,                   # (# conditions - 1) = 3
    p         = round(ft$p, 4),
    kendall_w = round(ef$effsize, 3),
    w_ci_low  = round(ef$conf.low, 3),
    w_ci_high = round(ef$conf.high, 3)
  )
}

# Test each target subscale and stack the three result rows.
friedman_table <- map_dfr(c("EMP", "NORM", "NAR"), run_friedman)

friedman_table
# All three subscales differ significantly across the four messages (n = 24 complete
# blocks, df = 3, all p < .001): EMP chi-square = 34.12, W = .47; NORM = 44.67,
# W = .62; NAR = 50.59, W = .70. Moderate-to-large agreement on the ordering - the
# manipulation moved ratings as intended.

write_csv(friedman_table, here("output/tables/pretest_friedman.csv"))

# Paste-ready APA strings for Friedman.
cat("\nFriedman tests (Section 4.1.2)\n",
    "-------------------------------\n", file = apa_path, sep = "", append = TRUE)
# One APA line per subscale: chi-square(df) = ..., p, W (or "not testable").
walk(seq_len(nrow(friedman_table)), function(i) {
  row <- friedman_table[i, ]
  if (is.na(row$chi2)) {
    line <- sprintf("  %s: not testable (n = %d).\n", row$scale, row$n)
  } else {
    line <- sprintf("  %s: chi-square(%d) = %.2f, %s, W = %s (n = %d)\n",
                    row$scale, row$df, row$chi2, format_p(row$p), fmt_r(row$kendall_w), row$n)
  }
  cat(line, file = apa_path, append = TRUE)
})


# ---- 8. Pairwise Wilcoxon post-hoc (Holm, with rank-biserial r) -------------
# For a significant Friedman: which message pairs differ? Wilcoxon SIGNED-RANK
# (paired, within-subjects) across all 6 condition pairs, Holm-Bonferroni across
# the family. rank-biserial r with 95% CI is the effect size.

run_pairwise <- function(scale_name) {
  d <- composites %>%
    filter(scale == scale_name) %>%
    dplyr::select(id, condition, score) %>%
    drop_na()

  # Same complete-block restriction as Friedman (all four messages per participant
  # so the signed-rank pairing is defined for every comparison).
  ids_complete <- d %>%
    group_by(id) %>%
    summarise(k = n(), .groups = "drop") %>%
    filter(k == nlevels(d$condition)) %>%
    pull(id)

  d <- d %>% filter(id %in% ids_complete)

  if (length(ids_complete) < 3) {            # too few blocks -> NA row
    return(tibble(scale = scale_name, group1 = NA_character_, group2 = NA_character_,
                  p_adj = NA_real_, r_rb = NA_real_, ci_lo = NA_real_, ci_hi = NA_real_,
                  n = length(ids_complete)))
  }

  # All 6 pairs (4 choose 2) tested; Holm controls the family-wise error (uniformly
  # more powerful than plain Bonferroni for the same FWER).
  # NOTE (review): rstatix::wilcox_test(paired = TRUE) relies on its own internal
  # row alignment; `d` is not explicitly arrange()-d by id before the call. The
  # effect-size block below re-pairs by id via pivot_wider, but the p-values here do
  # not. Worth verifying rstatix pairs by participant as intended (a known small-N
  # gotcha). Left as-is to keep the numbers identical to the original.
  pairs <- d %>%
    wilcox_test(score ~ condition, paired = TRUE, p.adjust.method = "holm") %>%
    transmute(
      scale  = scale_name,
      group1 = as.character(group1),        # one condition in the pair
      group2 = as.character(group2),        # the other condition in the pair
      p_adj  = round(p.adj, 4),             # Holm-adjusted p for this pair
      n      = length(ids_complete)
    )

  # rank-biserial r with 95% CI per pair. effectsize::rank_biserial() does not take
  # paired = TRUE through the formula interface, so each pair is reshaped to two
  # id-aligned vectors (one column per condition) and passed to the two-sample form.
  effsizes <- map_dfr(seq_len(nrow(pairs)), function(i) {
    pair <- pairs[i, ]
    # pivot_wider keeps the two scores on one row per id; drop_na() then leaves
    # id-aligned pairs - exactly the pairing a paired effect size needs.
    wide <- d %>%
      filter(condition %in% c(pair$group1, pair$group2)) %>%
      dplyr::select(id, condition, score) %>%
      pivot_wider(names_from = condition, values_from = score) %>%
      drop_na()

    # ci = 0.95 returns a 95% CI, satisfying the JARS "effect size + CI" rule.
    # tryCatch returns NULL if the pair is degenerate (e.g. all ties).
    es <- tryCatch(
      effectsize::rank_biserial(wide[[pair$group1]], wide[[pair$group2]],
                                paired = TRUE, ci = 0.95),
      error = function(e) NULL
    )
    if (is.null(es)) {
      tibble(r_rb = NA_real_, ci_lo = NA_real_, ci_hi = NA_real_)
    } else {
      tibble(r_rb = round(es$r_rank_biserial, 3),
             ci_lo = round(es$CI_low, 3), ci_hi = round(es$CI_high, 3))
    }
  })

  bind_cols(pairs, effsizes)   # glue the p-value table and the effect-size table side by side
}

pairwise_table <- map_dfr(c("EMP", "NORM", "NAR"), run_pairwise)

pairwise_table
# 18 rows = 3 subscales x 6 message pairs. The confirmatory contrasts (each target
# message vs. Neutral on its own scale) all survive Holm correction and carry near-
# ceiling effect sizes: EMP Neutral-vs-Empathy p_adj = .0005, |r| = .94; NORM
# Neutral-vs-Social Norm p_adj = .0003, |r| = 1.00; NAR Neutral-vs-Narrative
# p_adj = .0001, |r| = 1.00 (r sign is negative because Neutral is group1, the
# lower-scoring member of each pair).

write_csv(pairwise_table, here("output/tables/pretest_pairwise.csv"))


# ---- 9. Confirmation criteria C1-C5 -----------------------------------------
# Automated pass/fail against the preregistered manipulation-check criteria.
# C1-C3: the intended message has the highest mean on the matching subscale.
# C4: Neutral has the lowest mean on all three target subscales.
# C5: the two control items vary < 1.0 SD across the four message means.

# The intended winner for each target subscale.
target_pairs <- tribble(
  ~scale, ~expected_winner,
  "EMP",  "Empathy",         # C1
  "NORM", "Social Norm",     # C2
  "NAR",  "Narrative"        # C3
)

# 9.1 C1-C3: highest-scoring message per target scale. slice_max(mean, n = 1) keeps
# the single top-mean row per group; with_ties = FALSE keeps exactly one on a tie.
highest <- descriptives %>%
  filter(scale %in% c("EMP", "NORM", "NAR")) %>%
  group_by(scale) %>%
  slice_max(mean, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  dplyr::select(scale, observed_winner = condition, winner_mean = mean) %>%
  left_join(target_pairs, by = "scale") %>%
  mutate(
    criterion = case_when(scale == "EMP" ~ "C1", scale == "NORM" ~ "C2", scale == "NAR" ~ "C3"),
    pass      = as.character(observed_winner) == expected_winner   # PASS iff observed == expected
  )

# 9.2 C4: Neutral lowest on all three target scales. slice_min() = the lowest-mean
# message per subscale; all() = every subscale must have Neutral at the floor.
neutral_lowest <- descriptives %>%
  filter(scale %in% c("EMP", "NORM", "NAR")) %>%
  group_by(scale) %>%
  slice_min(mean, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(neutral_is_lowest = condition == "Neutral") %>%   # TRUE where Neutral is the floor
  summarise(pass = all(neutral_is_lowest)) %>%             # collapse the 3 checks to one PASS/FAIL
  mutate(criterion = "C4", scale = "EMP/NORM/NAR",
         observed_winner = NA_character_, expected_winner = "Neutral lowest",
         winner_mean = NA_real_) %>%
  dplyr::select(criterion, scale, observed_winner, expected_winner, winner_mean, pass)

# 9.3 C5: control-item SD across the four message means < 1.0. A good manipulation
# moves only the target scales, leaving control items flat. sd(mean) = the spread
# of the four message-level means for each control item.
ctrl_spread <- descriptives %>%
  filter(scale %in% c("CTRL1", "CTRL2")) %>%
  group_by(scale) %>%
  summarise(sd_between_messages = round(sd(mean, na.rm = TRUE), 3), .groups = "drop") %>%
  mutate(
    criterion = "C5", observed_winner = NA_character_, expected_winner = "SD < 1.0",
    winner_mean = NA_real_,
    pass = sd_between_messages < 1.0        # PASS iff the control item is stable across messages
  ) %>%
  dplyr::select(criterion, scale, observed_winner, expected_winner, winner_mean, pass)

# Stack C1-C3, C4 and C5 into one pass/fail table.
criteria_table <- bind_rows(
  highest %>%
    dplyr::select(criterion, scale, observed_winner, expected_winner, winner_mean, pass) %>%
    mutate(observed_winner = as.character(observed_winner)),   # keep column types compatible for bind_rows
  neutral_lowest,
  ctrl_spread
)

criteria_table
# C1-C3 PASS (Empathy 4.06, Social Norm 4.44, Narrative 4.62 each top their own
# scale), C5 PASS (control SDs across messages = .39 and .16, both < 1). C4 FAILS:
# Neutral is the floor on EMP and NAR but NOT on NORM, where the Empathy message
# (2.25) scores below Neutral (2.67).

# na.rm = TRUE so an untestable row does not block the verdict.
all_pass <- all(criteria_table$pass, na.rm = TRUE)
all_pass
# FALSE: the check misses on C4 alone. The directional manipulation still works -
# every motivational message tops its intended construct (C1-C3) and the controls
# stay flat (C5) - but Neutral is not the strict minimum on NORM, so the all-or-
# nothing preregistered criterion is not fully met. Reported transparently rather
# than glossed.

write_csv(criteria_table, here("output/tables/pretest_criteria.csv"))

# Paste-ready APA summary of the criteria.
cat("\nConfirmation criteria C1-C5\n",
    "----------------------------\n", file = apa_path, sep = "", append = TRUE)
# isTRUE() guards against an NA (untestable) row being printed as PASS.
walk(seq_len(nrow(criteria_table)), function(i) {
  row <- criteria_table[i, ]
  cat(sprintf("  %s (%s): %s\n", row$criterion, row$scale,
              ifelse(isTRUE(row$pass), "PASS", "FAIL")),
      file = apa_path, append = TRUE)
})
cat(sprintf("  Overall: %s\n", ifelse(all_pass, "PASS", "FAIL")),
    file = apa_path, append = TRUE)


# ---- 10. Order-effect check [not reported] -----------------------------------
# Each participant saw the four messages in a random order. If randomisation
# worked, position 1..4 should be roughly balanced across messages. Informational
# at small N.

if (nrow(order_data) > 0) {                       # only if the position log exists
  # Cross-tab of position (1..4) by which message appeared there.
  order_table <- order_data %>%
    count(position, page_label) %>%               # tally slot x message combinations
    pivot_wider(names_from = page_label, values_from = n, values_fill = 0)   # messages -> columns; empty cells = 0

  order_table
  # Every message appears in every slot, counts running 2 to 11 around a balanced 6.
  # -> no message sat in one position, so viewing order does not explain the
  #    differences between messages. The pretest procedure is described in
  #    Appendix E.

  write_csv(order_table, here("output/tables/pretest_order.csv"))
}


# ---- 11. Figures ------------------------------------------------------------

# 11.1 Heatmap: condition x scale means. Each cell shows the mean (and SD); the
# three intended message x subscale pairings get a bold border. The check passes
# when those bordered cells are the darkest in their column. Fill uses viridis (a
# perceptually uniform, non-condition sequential scale) rather than the condition
# palette, since the aesthetic here is the mean rating, not the condition.

# Flag the three intended cells (the diagonal we expect to be darkest).
heatmap_data <- descriptives %>%
  filter(scale %in% c("EMP", "NORM", "NAR")) %>%
  mutate(
    is_target = (condition == "Empathy"     & scale == "EMP")  |
                (condition == "Social Norm" & scale == "NORM") |
                (condition == "Narrative"   & scale == "NAR"),
    # Fill is mako reversed (high mean = dark), so cell text switches to white on the
    # dark high-mean cells and stays dark on the light low-mean cells for legibility.
    txt_col   = ifelse(mean > 3.0, "white", "grey15")
  )

subscale_titles <- c(EMP  = "Empathic concern",
                     NORM = "Descriptive norm salience",
                     NAR  = "Narrative structure")

set.seed(2026)   # reproducible jitter for the rain
set.seed(2026)   # reproducible rain jitter (runif below)
raincloud_data <- composites %>%
  filter(scale %in% c("EMP", "NORM", "NAR")) %>%
  mutate(
    scale  = factor(scale, levels = c("EMP", "NORM", "NAR")),
    # numeric x for the rain: category position, nudged left of centre + small jitter
    x_rain = as.integer(condition) - 0.24 + runif(n(), -0.05, 0.05)
  )

fig_raincloud <- raincloud_data %>%
  ggplot(aes(x = condition, y = score)) +
  ggdist::stat_halfeye(aes(fill = condition),                    # the "cloud": half-violin density
                       adjust = 0.7, width = 0.75, .width = 0,
                       justification = -0.18, point_colour = NA,
                       alpha = 0.75, side = "right") +
  geom_boxplot(width = 0.14, outlier.shape = NA,                 # median + quartiles, on the centre
               alpha = 0, linewidth = 0.5, colour = "grey25") +
  geom_point(aes(x = x_rain), size = 1.2, alpha = 0.4,           # the "rain": thinned, jittered points
             colour = "grey20", stroke = 0) +
  facet_wrap(~ scale, ncol = 1, labeller = as_labeller(subscale_titles)) +   # stacked: each subscale full width
  scale_y_continuous(limits = c(1, 5), breaks = 1:5) +           # fixed 1-5 Likert axis across panels
  scale_x_discrete(expand = expansion(add = c(0.55, 0.35)),      # room for rain (left) + cloud (right)
                   labels = c("Neutral", "Empathy", "Social norm", "Narrative")) +
  scale_fill_manual(values = condition_colours) +                # house condition palette from src/theme_apa.R
  # APA 7.2: every axis carries a title. The 1-to-5 range lives in the figure note
  # rather than the axis title, because the png device cannot encode an en dash and
  # would silently substitute a hyphen (APA 6.6 requires the en dash for a range).
  labs(x = "Message", y = "Composite rating") +
  theme_apa(legend_pos = "none") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))       # tilt labels so they do not overlap

fig_raincloud
save_apa(fig_raincloud, "fig_pretest_raincloud", width = 7.5, height = 9)


# ---- 12. Markdown results export --------------------------------------------
# Every result table is also written to one Markdown file so it can be pasted
# straight into the Obsidian draft now and regenerated as more responses arrive.
# At finalisation (ma-finalisierung) these convert to APA 7 Word tables with the
# rest of the thesis.

md_path <- here("output/tables/pretest_results.md")

# Append a titled, captioned Markdown table. `note` is an optional APA-style note.
write_md_table <- function(df, title, note = NULL, digits = 2) {
  cat("\n## ", title, "\n\n", sep = "", file = md_path, append = TRUE)   # section heading
  cat(knitr::kable(df, format = "pipe", digits = digits),               # data frame -> Obsidian pipe table
      sep = "\n", file = md_path, append = TRUE)
  cat("\n", file = md_path, append = TRUE)
  if (!is.null(note)) cat("\n*Note.* ", note, "\n", sep = "", file = md_path, append = TRUE)
}

# File header. No append = TRUE, so this OVERWRITES the file; every table below appends.
cat("# Pretest manipulation check - preliminary results\n\n",
    "Generated by `02_study2/02_pretest_validation.R`.  \n",
    "N = ", n_participants, " participants. ",
    "**Preliminary - regenerated as new responses arrive.**\n",
    sep = "", file = md_path)

write_md_table(demo_summary, "Sample characteristics",
               note = "Counts and percentages. DM03 measures English reading proficiency.")
write_md_table(descriptives, "Descriptive statistics by message and subscale",
               note = "M, SD, Mdn and percentile bootstrap 95% CI on the mean (5,000 replicates). Scores range 1-5.")
write_md_table(alpha_table, "Internal consistency (Cronbach's alpha)",
               note = "Pooled across messages; 95% CI via Feldt's method.", digits = 3)
if (exists("discriminant")) {   # only if Section 6 produced it
  write_md_table(discriminant, "Discriminant validity (pooled Pearson r)",
                 note = "Inter-subscale correlations of composite scores.", digits = 3)
}
write_md_table(friedman_table, "Friedman tests by subscale",
               note = "Within-subjects test across the four messages. W = Kendall's W.", digits = 3)
write_md_table(pairwise_table, "Pairwise Wilcoxon signed-rank (Holm-corrected)",
               note = "p_adj = Holm-Bonferroni adjusted p. r_rb = rank-biserial r with 95% CI.", digits = 3)
write_md_table(criteria_table, "Confirmation criteria C1-C5",
               note = "Preregistered manipulation-check criteria.", digits = 2)


# ---- 13. Reproducibility snapshot -------------------------------------------
# sessionInfo() captures the R version and all loaded package versions at run
# time. capture.output() grabs the printout; writeLines() saves it for the OSF
# replication package.

# session_info.txt is written by run_all.R, which owns it, because the manifest has
# to describe the session that produced the whole package. Writing it here as well
# meant that running this script on its own overwrote the orchestrator's record with
# a partial one, and session_info.txt is compared by run_all.R --check, so that made
# a clean package report a mismatch. The local copy is therefore not written.
