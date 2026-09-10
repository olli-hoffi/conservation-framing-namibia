# 01_study1/10_reception_story_structure.R
#
# WHERE THIS BELONGS
#   In the thesis: Chapter 3, Results: Reception and Engagement
#   Produces: the narrative-structure paragraph, no table
#   Status: Exploratory. Study 1 carries no preregistration at all, so every
#   association here is descriptive and none of it tests a hypothesis that was
#   stated in advance.
#
# Does story structure predict engagement? (exploratory)
#
# The Brief Discussion and the General Discussion lean on narrative structure as
# the sector's unused resource. This script asks whether the codebook's Story
# structure variable (co-presence of problem and solution elements, NOT a
# completed narrative arc) is associated with engagement, on the same outcomes
# and the same covariate sets as the reported models:
#   (1) the negative-binomial comment model of 01_study1/08_reception_engagement.R,
#   (2) its organization-clustered version from 06b,
#   (3) the log likes-rate mixed model of 01_study1/11_reception_misuse.R.
# It also decomposes the raw Problem-only comment advantage into its senders and
# formats, because 30 posts from mostly one account cannot carry a frame reading.
#
# in:  data/study1/analysis_base.csv
# out: output/tables/eng_story_structure_models.csv,
#      output/tables/eng_story_structure_desc.csv,
#      output/tables/eng_story_structure_lrt.csv
suppressPackageStartupMessages({library(tidyverse); library(MASS); library(lme4); library(here)})

# set.seed(42) fixes the percentile bootstrap behind the Cohen's w interval below.
set.seed(42)

d <- read_delim(here("data/study1/analysis_base.csv"), delim = ";",
                show_col_types = FALSE) %>%
  filter(coded == 1) %>%                      # same gate as 01_study1/08_reception_engagement.R
  mutate(Likes = as.numeric(Likes)) %>%
  mutate(across(c(Comments, Likes, Followers), as.numeric),
         story = relevel(factor(m_Story_Structure), ref = "None"))

# ---- descriptive decomposition first: who supplies each story cell ----------
desc <- d %>%
  filter(!is.na(m_Story_Structure)) %>%
  group_by(m_Story_Structure) %>%
  summarise(n = n(),
            mean_comments   = mean(Comments),
            sd_comments     = sd(Comments),
            median_comments = median(Comments),
            iqr_low         = quantile(Comments, .25),
            iqr_high        = quantile(Comments, .75),
            top_org         = names(sort(table(Actor_Name), decreasing = TRUE))[1],
            top_org_n       = max(table(Actor_Name)),
            n_video         = sum(Post_Format == "Video"),
            .groups = "drop")
print(desc)
# The raw contrast is dramatic and misleading. Posts coded Problem only average
# 55.6 comments (SD = 84.9) against 1.9 (SD = 9.0) for posts with neither element,
# a thirtyfold gap. But there are only 30 such posts and the median is 54 against
# 0, so the mean rests on a handful of threads. The decomposition below shows who
# supplies them, which is why this table is read before any model.
# Problem-only: 30 posts, 17 of them Ocean Conservation Namibia, 16 video. The
# raw comment advantage of that cell is a sender/format profile, not a frame result.
write_csv(desc, here("output/tables/eng_story_structure_desc.csv"))

# ---- (1) NB comment model of 06, plus story ---------------------------------
dm <- d %>%
  filter(is.finite(Followers), Followers > 0,
         !is.na(f_Empathy), !is.na(f_Threat), !is.na(f_Scientific),
         !is.na(f_Protagonist), !is.na(f_Interactivity), !is.na(m_Story_Structure)) %>%
  mutate(Post_Format = relevel(factor(Post_Format), ref = "Image"),
         logF = log(Followers))
cat("posts entering the comment model:", nrow(dm), "\n")
# 881 of the 917 coded posts. The 36 dropped lack a story code or a usable
# follower count. Like visibility does not affect this comment-count model.

f0 <- Comments ~ Post_Format + f_Empathy + f_Threat + f_Efficacy +
        f_Scientific + f_Protagonist + f_Interactivity + logF
nb0 <- glm.nb(f0, data = dm)                          # the reported model
nb1 <- glm.nb(update(f0, . ~ . + story), data = dm)   # plus story structure

lrt <- anova(nb0, nb1)                                # does story add anything?
# anova.negbin() names its degrees-of-freedom column "   df", with leading
# spaces, so lrt$df silently returns NULL and every downstream use of it is
# empty. Match on the trimmed name instead.
lrt_stat <- lrt$`LR stat.`[2]
lrt_df   <- lrt[[which(trimws(names(lrt)) == "df")]][2]
lrt_p    <- lrt$`Pr(Chi)`[2]
lrt_n <- nrow(dm)                                     # N the chi-square is evaluated on
cat(sprintf("LRT story: chi2(%d, N = %d) = %.2f, p = %.3f\n", lrt_df, lrt_n, lrt_stat, lrt_p))

# Effect size beside the chi-square, because a test statistic alone says only
# whether an effect is distinguishable from zero, never how large it is.
# Two complementary readings:
#   w  = sqrt(chi2 / N), Cohen's conventional chi-square effect size. Benchmarks
#        are .10 small, .30 medium, .50 large.
#   dR2 = the gain in McFadden pseudo-R2 from adding story, measured against an
#        intercept-only negative binomial. It answers "how much more of the
#        outcome does the larger model explain?" on the models' own scale.
cohen_w <- sqrt(lrt_stat / lrt_n)

# Percentile bootstrap over posts for the Cohen's w interval, so the effect size is
# reported with its precision rather than as a bare point estimate. Same convention as
# the Cramer's V interval in 01_study1/12_taxonomy_subsample.R: 3000 resamples, 2.5th
# and 97.5th percentiles. Resamples that fail to fit either model return NA and drop out.
w_boot <- replicate(3000, {
  i  <- sample(nrow(dm), replace = TRUE)
  db <- dm[i, , drop = FALSE]
  if (nlevels(droplevels(db$story)) < nlevels(dm$story)) return(NA_real_)
  fit <- try({
    b0 <- suppressWarnings(glm.nb(f0, data = db))
    b1 <- suppressWarnings(glm.nb(update(f0, . ~ . + story), data = db))
    a  <- anova(b0, b1)
    sqrt(a$`LR stat.`[2] / nrow(db))
  }, silent = TRUE)
  if (inherits(fit, "try-error")) NA_real_ else fit
})
w_lo <- unname(quantile(w_boot, .025, na.rm = TRUE))
w_hi <- unname(quantile(w_boot, .975, na.rm = TRUE))
cat(sprintf("Cohen's w = %.3f, 95%% CI [%.3f, %.3f], %d of 3000 resamples usable\n",
            cohen_w, w_lo, w_hi, sum(!is.na(w_boot))))
nb_null <- glm.nb(Comments ~ 1, data = dm)               # intercept-only baseline
mcf     <- function(m) as.numeric(1 - logLik(m) / logLik(nb_null))
story_lrt <- tibble(
  test          = "LRT: story structure added to the negative-binomial comment model",
  chi2          = round(lrt_stat, 2),
  df            = lrt_df,
  N             = lrt_n,
  p             = round(lrt_p, 3),
  p_exact       = signif(lrt_p, 3),
  cohen_w       = round(cohen_w, 3),
  w_lo          = round(w_lo, 3),
  w_hi          = round(w_hi, 3),
  mcfadden_base = round(mcf(nb0), 4),
  mcfadden_full = round(mcf(nb1), 4),
  mcfadden_gain = round(mcf(nb1) - mcf(nb0), 4)
)
print(as.data.frame(story_lrt))
write_csv(story_lrt, here("output/tables/eng_story_structure_lrt.csv"))

ci1 <- suppressMessages(confint(nb1))                 # profile-likelihood CIs, as in 06
irr <- as.data.frame(cbind(est = coef(nb1), ci1)) %>%
  rownames_to_column("term") %>%
  filter(str_detect(term, "^story")) %>%
  transmute(model = "nb_glm", term,
            IRR = exp(est), lo = exp(`2.5 %`), hi = exp(`97.5 %`))

# ---- (2) organization-clustered version, as in 06b --------------------------
# Attempted for symmetry with 06b; with the three story dummies added the model
# returns a degenerate Hessian, so its estimates are reported only if it
# converges cleanly. The sender-adjusted reading rests on model (3) instead.
irr_cl <- tibble()
gnb <- tryCatch(glmer.nb(update(f0, . ~ . + story + (1 | Actor_Name)), data = dm,
                         control = glmerControl(optimizer = "bobyqa",
                                                optCtrl = list(maxfun = 2e5))),
                error = function(e) NULL, warning = function(w) NULL)
if (!is.null(gnb)) {
  co <- summary(gnb)$coefficients
  irr_cl <- tibble(term = rownames(co), est = co[, 1], se = co[, 2]) %>%
    filter(str_detect(term, "^story")) %>%
    transmute(model = "nb_clustered", term,
              IRR = exp(est), lo = exp(est - 1.96 * se), hi = exp(est + 1.96 * se))
} else cat("clustered NB did not converge cleanly; omitted\n")

# ---- (3) log likes-rate mixed model of 02, plus story -----------------------
d2 <- d %>%
  filter(Likes >= 0) %>%
  mutate(log_er = log((Likes + 1) / Followers),
         cap_z  = as.numeric(scale(cap_chars)),
         hash_z = as.numeric(scale(n_hashtags)),
         Post_Format = relevel(factor(Post_Format), ref = "Image")) %>%
  filter(is.finite(log_er), !is.na(f_Scientific), !is.na(f_Protagonist),
         !is.na(f_Onscreen_Text), !is.na(m_Story_Structure))
cat("posts entering the likes-rate model:", nrow(d2), "\n")

m2 <- lmer(log_er ~ Post_Format + f_Scientific + f_Empathy + f_Threat +
             f_Protagonist + f_Onscreen_Text + cap_z + hash_z + story + (1 | Actor_Name),
           data = d2, REML = TRUE)
fe <- summary(m2)$coefficients
ci2 <- suppressWarnings(confint(m2, method = "Wald"))
b_story <- as.data.frame(fe) %>% rownames_to_column("term") %>%
  filter(str_detect(term, "^story")) %>%
  transmute(model = "lmm_likes_rate", term,
            IRR = NA_real_, lo = NA_real_, hi = NA_real_,
            b = Estimate,
            b_lo = ci2[match(term, rownames(ci2)), 1],
            b_hi = ci2[match(term, rownames(ci2)), 2])

out <- bind_rows(irr, irr_cl) %>% mutate(b = NA_real_, b_lo = NA_real_, b_hi = NA_real_) %>%
  bind_rows(b_story) %>%
  bind_rows(tibble(model = "lrt_vs_base", term = paste0("story (3 df, N = ", lrt_n, ")"),
                   IRR = NA_real_, lo = NA_real_, hi = NA_real_,
                   b = lrt_stat, b_lo = NA_real_, b_hi = lrt_p))
print(as.data.frame(out %>% mutate(across(where(is.numeric), ~ round(., 3)))))
# No story configuration draws more comments than posts carrying neither element.
# Problem and solution together sits at IRR 0.86 [0.55, 1.37], solution only at
# 0.79 [0.52, 1.21], problem only at 1.42 [0.73, 2.90]. Every interval spans 1.
# The likes-rate mixed model agrees, and posts coded solution only sit slightly
# below the baseline there (b = -0.13, 95% CI [-0.25, -0.01]).
#
# Taken with the descriptives above, the raw Problem-only advantage is a sender
# effect and not a story effect. The clustered negative binomial does not converge
# cleanly with three story dummies added, so the sender-adjusted reading rests on
# the likes-rate mixed model instead, which does converge.
write_csv(out, here("output/tables/eng_story_structure_models.csv"))
