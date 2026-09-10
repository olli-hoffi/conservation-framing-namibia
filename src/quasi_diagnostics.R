# =============================================================================
# src/quasi_diagnostics.R
#
# Wedderburn's proportionality check for quasi-binomial fits.
#
# A quasi-binomial model multiplies the binomial variance by a single constant,
# Var(y) = phi * mu(1 - mu) / n. That is only a correction if the excess variance
# really is PROPORTIONAL to the binomial variance (Wedderburn, 1974). Reporting phi
# alone does not establish this, it only reports the constant that was assumed.
#
# The check is deterministic and draws no random numbers, so it can be inserted
# anywhere in a script without shifting a downstream bootstrap.
#
# Two departures are tested on the squared Pearson residuals, whose expectation is
# phi under proportionality and therefore should not depend on anything:
#   fitted  slope against the fitted proportion. A nonzero slope means the
#           dispersion varies with the rate itself.
#   trials  slope against the number of trials. This is the beta-binomial
#           signature: with a constant intra-cluster correlation rho the squared
#           Pearson residual grows about linearly in n, so a positive slope says a
#           beta-binomial (or a random effect) fits the variance better than a
#           single multiplier.
# =============================================================================

wedderburn_check <- function(model, label = "") {
  mu <- fitted(model)                 # fitted proportions
  n  <- weights(model, type = "prior")  # binomial trials per row
  if (is.null(n)) n <- rep(1, length(mu))
  s  <- residuals(model, type = "pearson")^2

  fit_slope <- function(x) {
    if (length(unique(x)) < 3) {
      return(c(slope = NA_real_, t = NA_real_, p = NA_real_, df = NA_real_))
    }
    aux <- stats::lm(s ~ x)
    cf <- summary(aux)$coefficients
    if (nrow(cf) < 2) {
      return(c(slope = NA_real_, t = NA_real_, p = NA_real_, df = NA_real_))
    }
    c(slope = cf[2, 1], t = cf[2, 3], p = cf[2, 4],
      df = stats::df.residual(aux))
  }

  a <- fit_slope(mu)
  b <- fit_slope(n)

  data.frame(
    model          = label,
    phi            = round(summary(model)$dispersion, 3),
    df_residual    = stats::df.residual(model),
    df_auxiliary   = as.integer(unname(a["df"])),
    slope_fitted   = round(unname(a["slope"]), 4),
    t_fitted       = round(unname(a["t"]), 4),
    p_fitted       = round(unname(a["p"]), 4),
    slope_trials   = signif(unname(b["slope"]), 3),
    t_trials       = round(unname(b["t"]), 4),
    p_trials       = round(unname(b["p"]), 4),
    # The verdict is deliberately conservative. With 8 residual df these slopes are
    # themselves poorly determined, so "no departure detected" means the check did
    # not fire, not that proportionality is established.
    verdict        = unname(ifelse(is.na(a["p"]) | is.na(b["p"]), "not testable",
                     ifelse(a["p"] < 0.05 | b["p"] < 0.05,
                            "departure detected", "no departure detected"))),
    stringsAsFactors = FALSE
  )
}
