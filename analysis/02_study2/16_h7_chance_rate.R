# =============================================================================
# 02_study2/16_h7_chance_rate.R
#
# WHERE THIS BELONGS
#   In the thesis: Chapter 4, Results: Replication Hypothesis (H7)
#   Produces: the prose statistic "met by chance in 42 of 576 cases (7.3%),
#   against 16.7% for the rank-correlation half alone". No table or figure.
#   Status: Descriptive support for the preregistered H7 criterion, not a test.
#
# Chance rate of the combined H7 criterion under random rank orders.
#
# H7 counts as supported when (a) the condition-level CTR rank order correlates
# across the two rounds at Kendall's tau >= .60 and (b) the neutral control
# ranks below at least two motivational conditions in both rounds. This script
# enumerates all 24 x 24 = 576 pairs of orderings of the four conditions and
# counts how many satisfy the combined criterion, and how many satisfy the
# tau half alone. Full enumeration, no simulation, no seed.
#
# Runtime: under a second.
#
# out: output/tables/study2_h7_chance_rate.csv                (the prose statistic)
# =============================================================================

library(tidyverse)
library(here)

conditions <- c("neutral", "empathy", "social_norm", "narrative")
motivational <- setdiff(conditions, "neutral")

# All 24 orderings of four ranks, base R, no extra dependency.
perms <- expand.grid(a = 1:4, b = 1:4, c = 1:4, d = 1:4) %>%
  filter(pmap_int(., ~ n_distinct(c(...))) == 4) %>%
  as.matrix()

# Rank vector for one permutation: position i holds the rank of conditions[i].
rank_of <- function(p) {
  r <- integer(4)
  r[p] <- seq_len(4)
  set_names(r, conditions)
}

ranks <- map(seq_len(nrow(perms)), ~ rank_of(perms[.x, ]))

# Neutral-rank rule for one round: neutral ranks below at least two
# motivational conditions, meaning at least two of them hold a better
# (numerically smaller) rank than neutral.
neutral_ok <- function(r) sum(r[motivational] < r["neutral"]) >= 2

grid <- expand_grid(i = seq_along(ranks), j = seq_along(ranks)) %>%
  mutate(
    tau        = map2_dbl(i, j, ~ cor(ranks[[.x]], ranks[[.y]], method = "kendall")),
    tau_ok     = tau >= .60,
    neutral_r1 = map_lgl(i, ~ neutral_ok(ranks[[.x]])),
    neutral_r2 = map_lgl(j, ~ neutral_ok(ranks[[.x]])),
    combined   = tau_ok & neutral_r1 & neutral_r2
  )

out <- tibble(
  pairs_total      = nrow(grid),
  tau_only_n       = sum(grid$tau_ok),
  tau_only_rate    = mean(grid$tau_ok),
  combined_n       = sum(grid$combined),
  combined_rate    = mean(grid$combined)
)

out
# 576 pairs; tau half alone: 96 (16.7%); combined criterion: 42 (7.3%).

write_csv(out, here("output/tables/study2_h7_chance_rate.csv"))
