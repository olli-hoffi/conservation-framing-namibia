# =============================================================================
# src/apa_format.R
# Small helpers to format numbers for APA-7 in-text reporting.
# =============================================================================

# p-value: no leading zero, 3 decimals, floored at "< .001" (APA 6.44).
fmt_p <- function(p) {
  case_when(
    p < .001 ~ "< .001",
    # formatC() gives e.g. "0.032"; sub() strips the leading zero -> ".032"
    # because APA p-values carry no leading zero (p can never exceed 1).
    TRUE     ~ sub("0\\.", ".", formatC(p, digits = 3, format = "f"))
  )
}

# Confidence interval: "[LL, UL]" with a fixed number of decimals.
fmt_ci <- function(ll, ul, digits = 2) {
  str_c("[", formatC(ll, digits = digits, format = "f"),
        ", ", formatC(ul, digits = digits, format = "f"), "]")
}

# Cohen's d / OR: KEEP the leading zero (these can exceed 1, e.g. "1.20").
fmt_d <- function(d, digits = 2) formatC(d, digits = digits, format = "f")

# Correlation / kappa: DROP the leading zero (cannot exceed |1|, so APA omits it).
fmt_r <- function(r, digits = 2) sub("0\\.", ".", formatC(r, digits = digits, format = "f"))
