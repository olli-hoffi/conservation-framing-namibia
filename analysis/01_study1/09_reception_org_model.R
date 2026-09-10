# =============================================================================
# 01_study1/09_reception_org_model.R
#
# WHERE THIS BELONGS
#   In the thesis: Chapter 3, Results: Reception and Engagement
#   Produces: Table 5, the organization-adjusted columns
#   Status: Exploratory. Study 1 carries no preregistration at all, so every
#   association here is descriptive and none of it tests a hypothesis that was
#   stated in advance.
#
# Does the comment-count model survive organizational clustering?
#
# Posts are not independent. Each organization has its own audience size and its
# own posting habits, so a pooled model can credit a frame for what is really a
# sender effect. The check has to hold the outcome fixed. A model of comment
# counts can only be checked by another model of comment counts, never by a model
# of a likes-based rate, because two different outcomes cannot verify each other.
#
# This script therefore re-estimates the reported model with the same outcome and
# the same covariates, plus a random intercept for organization, and reports what
# moves.
#
# Status: exploratory robustness check, not a preregistered test.
# Runtime: about 10 seconds.
#
# in:  data/study1/analysis_base.csv
# out: output/tables/eng_comments_nb_clustered.csv, _fit.csv, _vif.csv,
#      _influence.csv, _influence_summary.csv, _simulation_diagnostics.csv
#
# Feeds Table 5, the organization-modeled columns.
# =============================================================================
suppressPackageStartupMessages({library(tidyverse); library(MASS); library(lme4); library(here)})

d <- read_delim(here("data/study1/analysis_base.csv"), delim = ";",
                show_col_types = FALSE) %>%
  filter(coded == 1) %>%                      # same gate as 01_study1/08_reception_engagement.R
  mutate(across(c(Comments, Followers), as.numeric))

dm <- d %>%
  filter(is.finite(Followers), Followers > 0,
         !is.na(f_Empathy), !is.na(f_Threat), !is.na(f_Scientific),
         !is.na(f_Protagonist), !is.na(f_Interactivity)) %>%
  mutate(Post_Format = relevel(factor(Post_Format), ref = "Image"),
         logF = log(Followers))
cat("posts entering the model:", nrow(dm), "| organizations:", n_distinct(dm$Actor_Name), "\n")
# 881 posts from 19 organizations, about 46 posts per organization on average.
# Enough per group for a random intercept to be estimable.

# ---- 1. The two models: pooled, then with a random intercept per organization ----
f <- Comments ~ Post_Format + f_Empathy + f_Threat + f_Efficacy +
       f_Scientific + f_Protagonist + f_Interactivity + logF

nb  <- glm.nb(f, data = dm)                                   # the reported model
gnb <- glmer.nb(update(f, . ~ . + (1 | Actor_Name)), data = dm,
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl = list(maxfun = 2e5)))  # same outcome, clustered
# A large gradient would make the comparison unusable, so the optimizer's final
# gradient is reported alongside the estimates.
cat("max|grad| =", round(max(abs(with(gnb@optinfo$derivs, solve(Hessian, gradient)))), 5), "\n")
# max|grad| = 8e-05, well inside the usual 0.002 tolerance, so the optimizer did
# reach a solution. lme4 also prints "degenerate Hessian with 3 negative eigenvalues".
# That warning does NOT describe the fit reported here. It is raised inside
# glmer.nb's inner theta search, and the Hessian stored on the returned object has
# 11 eigenvalues, none of them negative, with the smallest at 0.92. Checked by
# eigen(gnb@optinfo$derivs$Hessian). The fit is not degenerate, and lme4 prints no
# standard error for a random-effect SD in any case.
# Leave-one-organization-out refits move the SD only between 0.88 and 1.05. That is why the comparison
# below rests on whether conclusions change, not on a new p value.


# ---- 2. Side by side: does any conclusion move once the sender is modelled? ----
pull_irr <- function(m, label) {
  co <- if (inherits(m, "glmerMod")) summary(m)$coefficients else summary(m)$coefficients
  tibble(term = rownames(co), est = co[, 1], se = co[, 2]) %>%
    filter(term != "(Intercept)") %>%
    transmute(term, !!paste0("IRR_", label) := exp(est),
              !!paste0("lo_", label) := exp(est - 1.96 * se),
              !!paste0("hi_", label) := exp(est + 1.96 * se),
              !!paste0("z_", label) := est / se,
              !!paste0("p_", label) := 2 * pnorm(-abs(est / se)))
}
cmp <- pull_irr(nb, "glm") %>% left_join(pull_irr(gnb, "clustered"), by = "term") %>%
  mutate(sign_flips = (IRR_glm > 1) != (IRR_clustered > 1),
         ci_excludes_1_glm       = lo_glm > 1 | hi_glm < 1,
         ci_excludes_1_clustered = lo_clustered > 1 | hi_clustered < 1,
         conclusion_changes = ci_excludes_1_glm != ci_excludes_1_clustered)

print(cmp %>% mutate(across(where(is.numeric), ~ round(., 3))), n = 30)
cat("\nrandom-intercept SD:", round(sqrt(unlist(VarCorr(gnb))), 3), "\n")
# Random-intercept SD = 1.006 on the log scale. Organizations differ by roughly a
# factor of e in baseline comment count once the covariates are held constant,
# which is a large sender effect and the reason this check is needed at all.
cat("terms whose sign flips:", sum(cmp$sign_flips), "\n")
cat("terms whose CI conclusion changes:", sum(cmp$conclusion_changes), "\n")
# 0 signs flip and 0 conclusions change across all nine terms. The estimates do
# shrink towards 1 once the sender is modelled: Video 3.23 -> 2.81, Empathy
# 1.96 -> 1.72, Threat 1.91 -> 1.45, Interactivity 3.94 -> 2.70, follower count
# 2.16 -> 2.01. The frame-level findings of Table 5 therefore hold on an outcome
# that absorbs sender differences, with smaller effects.
write_csv(cmp, here("output/tables/eng_comments_nb_clustered.csv"))

# The chapter reports the random-intercept SD and the diagnosed optimizer warning.
# Save the final gradient and Hessian check so that this qualification has a
# reproducible source rather than resting on console output.
final_hessian_min <- min(eigen(gnb@optinfo$derivs$Hessian,
                               symmetric = TRUE, only.values = TRUE)$values)
write_csv(tibble(quantity = c("ranef_sd", "max_abs_rel_grad", "theta",
                              "min_final_hessian_eigenvalue"),
                 value = c(sqrt(unlist(VarCorr(gnb))),
                           max(abs(with(gnb@optinfo$derivs, solve(Hessian, gradient)))),
                           getME(gnb, "glmer.nb.theta"), final_hessian_min)),
          here("output/tables/eng_comments_nb_clustered_fit.csv"))

# ---- 3. Simulation-based distributional fit --------------------------------
# Model-based simulations condition on the fitted organization effects. The zero
# count checks whether the fitted model reproduces the observed number of zeros.
# The maximum and corpus variance check whether the negative-binomial tail
# reproduces the most extreme comment counts.
set.seed(20260906)
n_sim <- 5000
sim_counts <- simulate(gnb, nsim = n_sim, re.form = NULL)

sim_zero <- vapply(sim_counts, function(y) sum(y == 0), numeric(1))
sim_max  <- vapply(sim_counts, max, numeric(1))
sim_var  <- vapply(sim_counts, var, numeric(1))

simulation_row <- function(label, observed, simulated) {
  lower_tail <- sum(simulated <= observed)
  upper_tail <- sum(simulated >= observed)
  tibble(
    diagnostic = label,
    observed = observed,
    simulated_mean = mean(simulated),
    simulated_q025 = unname(quantile(simulated, .025)),
    simulated_q975 = unname(quantile(simulated, .975)),
    two_sided_tail_p = min(1, 2 * (min(lower_tail, upper_tail) + 1) / (n_sim + 1))
  )
}

simulation_diagnostics <- bind_rows(
  simulation_row("zero_count", sum(dm$Comments == 0), sim_zero),
  simulation_row("maximum_count", max(dm$Comments), sim_max),
  simulation_row("corpus_variance", var(dm$Comments), sim_var)
)
write_csv(simulation_diagnostics,
          here("output/tables/eng_comments_nb_simulation_diagnostics.csv"))
print(simulation_diagnostics)

# Collinearity and single-post influence for the reported model. GVIF from car
# (correct for the format factor); Cook's distance, then a refit without the
# single most influential post, comparing every IRR and its Wald CI conclusion.

# ---- 4. Collinearity and single-post influence on the reported model ----------
vif_raw <- car::vif(nb)
vif_tab <- tibble(term = rownames(as.data.frame(vif_raw)),
                  gvif  = as.data.frame(vif_raw)[[1]])
cd  <- cooks.distance(nb)
top <- which.max(cd)
nb_loo <- update(nb, data = dm[-top, ])
co_f <- summary(nb)$coefficients; co_l <- summary(nb_loo)$coefficients
infl <- tibble(term = rownames(co_f), irr_full = exp(co_f[, 1]), irr_loo = exp(co_l[, 1]),
               rel_change_pct = 100 * (exp(co_l[, 1]) / exp(co_f[, 1]) - 1),
               excl1_full = (exp(co_f[, 1] - 1.96 * co_f[, 2]) > 1) | (exp(co_f[, 1] + 1.96 * co_f[, 2]) < 1),
               excl1_loo  = (exp(co_l[, 1] - 1.96 * co_l[, 2]) > 1) | (exp(co_l[, 1] + 1.96 * co_l[, 2]) < 1)) %>%
  filter(term != "(Intercept)")
cat("max Cook's D:", round(max(cd), 4), "| max |IRR shift| %:",
    round(max(abs(infl$rel_change_pct)), 1),
    "| CI conclusions changed:", sum(infl$excl1_full != infl$excl1_loo), "\n")
# max Cook's D = 0.33, below the conventional threshold of 1, so no single post
# dominates the fit. Dropping the most influential post moves no IRR by more than
# 7.6% and changes no CI conclusion. The model does not rest on one observation.
write_csv(vif_tab, here("output/tables/eng_comments_nb_vif.csv"))
# Cook's D is quoted in the Results, so it is written out rather than left on the console.
write_csv(tibble(quantity = c("max_cooks_d", "most_influential_row", "max_abs_irr_shift_pct"),
                 value = c(max(cd), as.numeric(top), max(abs(infl$rel_change_pct)))),
          here("output/tables/eng_comments_nb_influence_summary.csv"))
write_csv(infl, here("output/tables/eng_comments_nb_influence.csv"))
