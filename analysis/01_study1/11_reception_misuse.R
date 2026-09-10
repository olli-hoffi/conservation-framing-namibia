# =============================================================================
# 01_study1/11_reception_misuse.R
#
# WHERE THIS BELONGS
#   In the thesis: Chapter 3, Results: Reception and Engagement
#   Produces: Figure B1, Table B5
#   Status: Exploratory. Study 1 carries no preregistration at all, so every
#   association here is descriptive and none of it tests a hypothesis that was
#   stated in advance.
#
# Study 1 (content analysis, PURELY EXPLORATORY, no preregistration):
# "Instagram misuse" + engagement. Caption-heavy / image-light usage, the
# tell-vs-show gap, and engagement done RIGHT (rate = Likes / Followers, nested
# within organisation).
#
# WHY engagement RATE not raw likes: big-follower orgs collect more likes
# automatically, so raw likes mostly measure account size. Dividing by followers
# (Likes / Followers) puts a small and a large org on the same scale. Everything
# here is exploratory and descriptive; the associations are CORRELATIONAL, with
# no causal reading.
#
# Input : data/study1/analysis_base.csv   (master per-post table, semicolon-delimited)
# Output: output/tables/misuse_scientific_by_org.csv,
#         misuse_scientific_org_concentration.csv,
#         misuse_engagement_spearman.csv, misuse_cta_likes_rate.csv,
#         misuse_mixed_model.csv, misuse_cta_model_test.csv,
#         misuse_mixed_model_varcomp.csv, misuse_mixed_model_vif.csv,
#         misuse_mixed_model_diagnostics.csv (+ APA .docx);
#         output/figures/fig_misuse_lmm_diagnostics
# =============================================================================

# lme4 supplies lmer() for the organisation-nested mixed model.
pacman::p_load(here, tidyverse, lme4, flextable, officer)
list.files(here("src"), full.names = TRUE) %>% walk(source)

# delim = ";" because captions contain commas, so the export is semicolon-separated.
df <- read_delim(here("data/study1/analysis_base.csv"), delim = ";", show_col_types = FALSE)

# Keep coded posts only, and coerce the modelled columns up front: as.integer on the
# 0/1 frame flags, as.numeric on the continuous engagement fields.
d <- df %>% filter(coded == 1) %>%
  mutate(across(starts_with("f_"), ~ suppressWarnings(as.integer(.))),
         Likes     = as.numeric(Likes),
         eng_rate  = as.numeric(eng_rate),
         clip_align = as.numeric(clip_align),
         Followers = as.numeric(Followers))

nrow(d)
# 917 coded posts enter content descriptives.

d_like <- d %>% filter(Likes >= 0)
nrow(d_like)
# 913 posts have an observable like count and enter likes-based analyses.


# ---- Likes rate by call-to-action type ---------------------------------------
# Blank values in m_Primary_CTA are the codebook's "None" category. The grouped
# rows answer the broad request-versus-no-request question, while the type rows
# show whether that summary hides a low-performing request type. Medians are used
# because likes per follower are strongly right-skewed.
cta_likes <- d_like %>%
  mutate(CTA_Type = replace_na(m_Primary_CTA, "None"),
         CTA_Group = if_else(CTA_Type == "None", "No request", "Any request"))

cta_likes_rate <- bind_rows(
  cta_likes %>%
    group_by(category = CTA_Group) %>%
    summarise(n_posts = n(),
              median_likes_per_1000_followers = 1000 * median(eng_rate),
              mean_likes_per_1000_followers = 1000 * mean(eng_rate),
              .groups = "drop") %>%
    mutate(grouping = "Any request versus none", .before = 1),
  cta_likes %>%
    group_by(category = CTA_Type) %>%
    summarise(n_posts = n(),
              median_likes_per_1000_followers = 1000 * median(eng_rate),
              mean_likes_per_1000_followers = 1000 * mean(eng_rate),
              .groups = "drop") %>%
    mutate(grouping = "Primary CTA type", .before = 1)
) %>%
  mutate(across(ends_with("followers"), ~ round(.x, 2)))

write_csv(cta_likes_rate, here("output/tables/misuse_cta_likes_rate.csv"))
cta_likes_rate


# ---- 1. Caption load [not reported] ------------------------------------------
# How text-heavy the captions are. The median is the headline number because
# caption length is right-skewed.
caption_summary <- tibble(
  cap_chars_median = median(d$cap_chars),
  cap_chars_mean   = round(mean(d$cap_chars)),
  cap_words_median = median(d$cap_words)
)

caption_summary
# Median 473 characters (mean 549), median 69 words.
# -> text-heavy posts, which fits information carried in the caption rather than
#    the image. Caption length in words is reported corpus-wide, median 65.

# mean() of a 0/1 flag = the proportion of posts with that visual device present.
# ---- Visual-frame prevalence, recomputed [not reported] ----------------------
# Recounts four visual devices on the posts this script uses.
img_use <- d %>%
  summarise(across(c(f_Protagonist, f_Onscreen_Text, f_Data_Visual, f_Youth_Style),
                   ~ mean(., na.rm = TRUE)))

img_use %>% mutate(across(everything(), ~ round(.x, 3)))
# Protagonist 64.8%, on-image text 22.6%, youth style 1.6%, data visualization 0.5%.
# -> faces carry the visuals and data visualization is near absent. The same four
#    devices are reported with intervals and their own base in Table 3, so this
#    recount only guards the analyses that follow.


# ---- 2. Tell-vs-show gap [not reported] --------------------------------------
# "Tell vs show": do posts that use the Scientific frame (tell facts) actually back
# it with a data visual (show it)? A low share = they assert science but rarely
# visualise it.
sci <- d %>% filter(f_Scientific == 1)   # posts coded as using the Scientific frame
tell_vs_show <- tibble(
  scientific_posts = nrow(sci),
  with_data_visual = sum(sci$f_Data_Visual, na.rm = TRUE),
  pct_with_visual  = round(100 * mean(sci$f_Data_Visual, na.rm = TRUE), 1)
)

tell_vs_show
# 262 scientific-frame posts, 3 pair it with a data visual (1.2%).
# -> science is told and almost never shown, a gap between a claim and its
#    evidence. The corpus-wide version is reported, data visualizations in 0.5%
#    of posts against a third of captions citing numbers.

# Scientific framing is distributed across organizations but concentrated among
# three accounts whose mandates or recurring output are research-led. The table
# preserves every contributing account; the summary generates the concentration
# figures used to explain why sender and frame remain difficult to separate.
research_led_accounts <- c("Gobabeb Research & Training Centre",
                           "Giraffe Conservation Foundation",
                           "AfriCat Foundation")
scientific_total <- sum(d$f_Scientific == 1, na.rm = TRUE)
scientific_by_org <- d %>%
  group_by(Actor_Name) %>%
  summarise(n_posts = n(),
            n_scientific = sum(f_Scientific == 1, na.rm = TRUE),
            pct_org_posts_scientific = round(100 * mean(f_Scientific == 1, na.rm = TRUE), 1),
            .groups = "drop") %>%
  filter(n_scientific > 0) %>%
  mutate(pct_scientific_occurrences = round(100 * n_scientific / scientific_total, 1),
         research_led_account = Actor_Name %in% research_led_accounts) %>%
  arrange(desc(n_scientific))

scientific_org_concentration <- tibble(
  n_organizations_total = n_distinct(d$Actor_Name),
  n_organizations_with_scientific = nrow(scientific_by_org),
  n_scientific_occurrences = scientific_total,
  n_research_led_accounts = length(research_led_accounts),
  n_occurrences_research_led = sum(scientific_by_org$n_scientific[
    scientific_by_org$research_led_account]),
  pct_occurrences_research_led = round(100 * n_occurrences_research_led /
                                         n_scientific_occurrences, 1)
)

write_csv(scientific_by_org, here("output/tables/misuse_scientific_by_org.csv"))
write_csv(scientific_org_concentration,
          here("output/tables/misuse_scientific_org_concentration.csv"))
scientific_org_concentration


# ---- 3. Engagement rate by format [not reported] -----------------------------
# Median engagement rate per post format (median again because the rate is skewed).
# arrange() sorts the best-performing format first.
er_fmt <- d_like %>%
  group_by(Post_Format) %>%
  summarise(n = n(), rate_median = median(eng_rate), likes_median = median(Likes),
            .groups = "drop") %>%
  arrange(desc(rate_median))

er_fmt %>% mutate(rate_median = round(100 * rate_median, 2))   # rate expressed as a percentage
# Video 1.02%, carousel 1.00%, image 0.62%, p < .001.
# -> format matters for engagement, but the raw comparison carries the sender with
#    it, since some accounts post almost only video. Sender-adjusted estimates are
#    reported in Table B5.

# Kruskal-Wallis = non-parametric one-way ANOVA on ranks: is engagement rate
# distributed differently across formats? Used (not ANOVA) because the rate is
# skewed and variances differ; it tests medians/ranks, not means.
kruskal_format <- kruskal.test(eng_rate ~ factor(Post_Format), data = d_like)
kruskal_format$p.value
# p = 2.6e-11: engagement rate differs across formats (Image lowest), significant.


# ---- 4. Feature vs engagement RATE (Spearman, with p) -----------------------
# Spearman rank correlation between each caption/frame feature and engagement rate.
# Spearman (not Pearson) because the rate is heavily right-skewed and the frame
# features are 0/1; correlating RANKS assumes no normality or linearity.
feats <- c("cap_chars", "n_hashtags", "n_emojis", "clip_align",
           "f_Scientific", "f_Empathy", "f_Threat", "f_Protagonist",
           "f_Onscreen_Text", "f_Data_Visual", "f_Efficacy", "f_Economic")

# Percentile bootstrap CI on the rank correlation. cor.test gives no interval for
# Spearman, and the thesis reports effect sizes with intervals throughout, so the
# rho values get one here. Resample post pairs with
# replacement; 3000 replicates, matching boot_spearman in 01_study1/07_attention_species_places.R.
set.seed(2026)
boot_rho_ci <- function(x, y, n_resamples = 3000) {
  reps <- replicate(n_resamples, {
    i <- sample.int(length(x), replace = TRUE)
    suppressWarnings(cor(x[i], y[i], method = "spearman"))
  })
  quantile(reps, c(0.025, 0.975), na.rm = TRUE, names = FALSE)
}

# For each feature keep rows where both it and eng_rate are present, then correlate.
corr <- map_dfr(feats, function(v) {
  ok <- complete.cases(d_like[[v]], d_like$eng_rate)                                  # drop NA pairs
  ct <- suppressWarnings(cor.test(d_like[[v]][ok], d_like$eng_rate[ok], method = "spearman"))
  ci <- boot_rho_ci(d_like[[v]][ok], d_like$eng_rate[ok])
  tibble(feature = v, rho = unname(ct$estimate),                                      # rho = correlation
         ci_lo = ci[1], ci_hi = ci[2], n = sum(ok), p = ct$p.value)                    # p = its test
}) %>% arrange(desc(rho))

corr %>% mutate(rho = round(rho, 3), p = signif(p, 2))
# Positive: f_Scientific rho = .22 (p = 2.5e-11), f_Threat .10 (p = .002). Negative:
# n_hashtags -.18 (p = 3.3e-08), clip_align -.08 (p = .011), n_emojis -.07 (p = .036).
# Scientific- and threat-framed posts correlate with higher engagement; hashtag-heavy,
# tightly image-aligned, emoji-rich posts with lower. Correlational only.

write_csv(corr, here("output/tables/misuse_engagement_spearman.csv"))
# [not reported, partial] cap_chars, n_hashtags, f_Scientific, f_Empathy, f_Threat,
# f_Protagonist and f_Onscreen_Text carry into Table B5. The n_emojis, clip_align,
# f_Data_Visual, f_Efficacy and f_Economic rows do not.


# ---- 5. Org-nested model: log engagement rate ~ format + frames + (1|org) ----
# Mixed-effects model. The DV is log((Likes + 1) / Followers): the log tames the
# skew and +1 avoids log(0). (1 | Actor_Name) is a random intercept per organisation,
# which accounts for the fact that posts from the same org are not independent
# (nesting). The fixed effects then estimate feature effects ADJUSTED for org-level
# baseline.
d2 <- d_like %>%
  mutate(log_er = log((Likes + 1) / Followers),
         cap_z  = as.numeric(scale(cap_chars)),     # z-standardise so its coefficient is per-SD
         hash_z = as.numeric(scale(n_hashtags)),    # z-standardise hashtag count likewise
         Post_Format = relevel(factor(Post_Format), ref = "Image"),       # Image = reference category
         CTA_Type = relevel(factor(replace_na(m_Primary_CTA, "None")),
                            ref = "None")) %>%                             # no request = reference
  filter(is.finite(log_er), !is.na(f_Scientific), !is.na(f_Protagonist),  # complete cases for the model terms
         !is.na(f_Onscreen_Text))

nrow(d2)
# 875 posts enter the model (finite log-rate + complete frame flags).

# REML = TRUE gives less biased variance-component estimates (standard for
# interpreting, not comparing, a mixed model).
m <- lmer(log_er ~ Post_Format + CTA_Type + f_Scientific + f_Empathy + f_Threat +
            f_Protagonist + f_Onscreen_Text + cap_z + hash_z + (1 | Actor_Name),
          data = d2, REML = TRUE)

# CTA type is assessed as an additional fixed-effect block. Fixed-effect model
# comparisons use maximum likelihood rather than REML. This tests whether the CTA
# block improves fit after the same sender and content adjustments as Table B5.
m_cta_ml <- update(m, REML = FALSE)
m_no_cta_ml <- update(m_cta_ml, . ~ . - CTA_Type)
cta_lrt_raw <- anova(m_no_cta_ml, m_cta_ml)
cta_model_test <- tibble(
  comparison = "Primary CTA type added to organization-adjusted likes model",
  df_added = cta_lrt_raw$npar[2] - cta_lrt_raw$npar[1],
  chisq = cta_lrt_raw$Chisq[2],
  p = cta_lrt_raw$`Pr(>Chisq)`[2],
  n_posts = nobs(m_cta_ml)
)
write_csv(cta_model_test, here("output/tables/misuse_cta_model_test.csv"))
cta_model_test

fe <- summary(m)$coefficients                        # fixed-effect estimates + t values
ci <- suppressWarnings(confint(m, method = "Wald"))  # Wald 95% CIs (fast approximation) for the fixed effects

# Collinearity among the fixed effects. The comment model of Table 5 reports
# generalized VIFs from 01_study1/09_reception_org_model.R, and the likes model of
# Table B5 reports the equivalent here, so a reader can judge whether its
# coefficients are separable. GVIF is reported because Post_Format is a
# multi-level factor: for such terms car::vif returns GVIF^(1/(2*df)), whose square
# is the quantity comparable to an ordinary VIF.
vif_lmm <- car::vif(m)
vif_lmm_tab <- if (is.matrix(vif_lmm)) {
  tibble(term = rownames(vif_lmm),
         gvif = round(vif_lmm[, "GVIF^(1/(2*Df))"]^2, 3))
} else {
  tibble(term = names(vif_lmm), gvif = round(vif_lmm, 3))
}
write_csv(vif_lmm_tab, here("output/tables/misuse_mixed_model_vif.csv"))
cat("\n--- Collinearity of the likes mixed model (Table B5) ---\n")
print(as.data.frame(vif_lmm_tab))
cat("max comparable VIF: ", max(vif_lmm_tab$gvif), "\n", sep = "")

# Assemble a tidy table: drop the intercept, attach each term's CI by name-matching.
mixed_model <- as.data.frame(fe) %>% rownames_to_column("term") %>%
  filter(term != "(Intercept)") %>%
  mutate(ci_lo = ci[match(term, rownames(ci)), 1],
         ci_hi = ci[match(term, rownames(ci)), 2]) %>%
  dplyr::select(term, Estimate, SE = `Std. Error`, ci_lo, ci_hi, t = `t value`)

mixed_model %>% mutate(across(where(is.numeric), ~ round(., 3)))
# Estimate = effect on the log engagement rate, org-adjusted; a CI that excludes 0
# is robust to org-level baseline. Post_FormatVideo +0.38 [0.24, 0.52], Carousel
# +0.31 [0.20, 0.42], f_Empathy +0.35 [0.21, 0.48], f_Threat +0.22 [0.09, 0.35] and
# f_Protagonist +0.13 [0.02, 0.23] lift the rate; f_Onscreen_Text -0.20 [-0.32, -0.07]
# and cap_z -0.11 [-0.17, -0.05] lower it (f_Scientific and hash_z ns). Empathy /
# threat framing and richer formats still pay off after controlling for which org posted.

write_csv(mixed_model, here("output/tables/misuse_mixed_model.csv"))

# Variance components of the mixed model, written out because the chapter reports
# the random-intercept SD and a script comment is not a source.
vc <- as.data.frame(VarCorr(m)) %>%
  dplyr::select(grp, var1, sdcor)
write_csv(vc, here("output/tables/misuse_mixed_model_varcomp.csv"))

# Residual and random-effect diagnostics for the mixed model, written out because
# the chapter reports an assumptions sentence and needs a source. Moments are
# computed manually to avoid an extra package; excess kurtosis = m4/m2^2 - 3.
z  <- resid(m) / sd(resid(m))
re <- ranef(m)$Actor_Name[["(Intercept)"]]
sw <- shapiro.test(re)
diag_tbl <- tibble(
  quantity = c("resid_skewness", "resid_excess_kurtosis", "max_abs_std_resid",
               "ranef_shapiro_W", "ranef_shapiro_p", "n_resid", "n_ranef"),
  value    = c(mean(z^3), mean(z^4) - 3, max(abs(z)),
               as.numeric(sw$statistic), sw$p.value, length(z), length(re)))
write_csv(diag_tbl, here("output/tables/misuse_mixed_model_diagnostics.csv"))

# Diagnostic figure for Appendix E: QQ plot (normality) and residuals against
# fitted values (homoscedasticity), both on standardized residuals.
pacman::p_load(patchwork)
p_qq <- ggplot(tibble(z = z), aes(sample = z)) +
  stat_qq(size = 0.7, alpha = 0.5) +
  stat_qq_line(linewidth = 0.4) +
  labs(x = "Theoretical Quantiles", y = "Standardized Residuals") +
  theme_apa()
p_rf <- ggplot(tibble(f = fitted(m), z = z), aes(f, z)) +
  geom_point(size = 0.7, alpha = 0.5) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  labs(x = "Fitted Values (Log Likes Rate)", y = "Standardized Residuals") +
  theme_apa()
# theme_apa blanks plot.title, so panel labels go in as patchwork tags.
save_apa((p_qq + p_rf) + plot_annotation(tag_levels = "A") &
           theme(plot.tag = element_text(size = 11, face = "bold")),
         "fig_misuse_lmm_diagnostics", width = apa_width_full, height = 4)
save_apa_table(mixed_model, "misuse_mixed_model",
               title = "Table. Org-Nested Mixed Model of Log Engagement Rate",
               note  = "Estimate = effect on log((Likes + 1) / Followers), org-adjusted. CI excluding 0 = robust.",
               digits = 3)


# ---- 6. Org communication-style profiles [not reported] ----------------------
# One profile row per org that posted >= 15 times (fewer posts = noisy per-org
# medians). Percent columns = share of that org's posts using each device; eng_rate
# as a percentage.
org_profiles <- d_like %>%
  group_by(Actor_Name) %>% filter(n() >= 15) %>%
  summarise(n = n(), followers = first(Followers),
            cap_len      = median(cap_chars),
            onscreen_pct = round(100 * mean(f_Onscreen_Text, na.rm = TRUE)),
            protag_pct   = round(100 * mean(f_Protagonist, na.rm = TRUE)),
            datavis_pct  = round(100 * mean(f_Data_Visual, na.rm = TRUE)),
            eng_rate_pct = round(100 * median(eng_rate), 2),
            .groups      = "drop") %>%
  arrange(desc(cap_len))   # longest-caption orgs first

org_profiles
# 15 organizations clear the floor. Caption length runs from 1,007 characters
# (AfriCat Foundation) to 108 (Ocean Conservation Namibia).
# -> style varies independently of size, the longest-caption accounts are not the
#    largest. Style is reported through the cluster analysis, which separated no
#    groups.
