# =============================================================================
# 02_study2/15_comment_reliability.R
#
# WHERE THIS BELONGS
#   In the thesis: Chapter 4, Results: Exploratory Analyses
#   Produces: Table 15
#   Status: Exploratory. Study 2 has a preregistration, but this script is not
#   part of it. Only 04_confirmatory.R and 05_confirmatory_h2a.R are, and any
#   result here is a finding to be retested rather than a test.
#
# Reliability of the advertisement-comment stance coding, all of Table 15.
#
# Each of the 71 comments received three readings. The author coded with the
# framing condition visible, an independent second coder saw only the comment
# text, and a language model saw neither the condition nor the other codes. This
# script quantifies how far those three readings agree.
#
# Status: exploratory. The comment analysis is not part of the preregistration.
# Runtime: about 2 seconds.
#
# in:  data/study2/second_coding/coding_comparison_3way.csv
#      data/study2/second_coding/second_coder_codes.csv
# out: output/tables/study2_comment_overall_agreement.csv   (Table 15, row 1)
#      output/tables/study2_comment_pairwise_agreement.csv  (Table 15, rows 2 to 7)
#      output/tables/study2_comment_category_agreement.csv  (per-category diagnostic)
# =============================================================================

pacman::p_load(here, tidyverse)
list.files(here("src"), full.names = TRUE) %>% walk(source)

# Coder 1 must be the ORIGINAL codes. MetaAds_comments_coding.csv holds coder 1
# AFTER consensus resolution, and comparing that against coder 2 returns 80.3% and
# kappa .72 because the resolution has already absorbed coder 2's reading. The
# pre-resolution figure below is the defensible one and the one the thesis reports.
three <- read_delim(here("data/study2/second_coding/coding_comparison_3way.csv"),
                    delim = ";", show_col_types = FALSE)
second <- read_delim(here("data/study2/second_coding/second_coder_codes.csv"),
                     delim = ";", show_col_types = FALSE)

# The first coder folds everything non-substantive into "other", so the labels are
# aligned before anything is compared.
align <- function(x) {
  x <- str_trim(str_to_lower(as.character(x)))
  recode(x, "off-topic" = "other", "offtopic" = "other", "spam" = "other",
            "misinformation" = "disputing")
}

# Cohen's kappa written out rather than taken from a package, so the coefficient is
# visible at the point of use and matches the per-category variant below exactly.
# Expected agreement of 1 leaves kappa undefined, which happens when both raters use
# a single category throughout. NA is returned there instead of a misleading zero.
cohen_kappa <- function(a, b) {
  n  <- length(a)
  po <- mean(a == b)
  cats <- base::union(a, b)
  pe <- sum(vapply(cats, function(c) mean(a == c) * mean(b == c), numeric(1)))
  if (pe >= 1) NA_real_ else (po - pe) / (1 - pe)
}
# The interval Table 15 reports. Kappa's large-sample variance is the one in Fleiss,
# Cohen and Everitt (1969), written out here for the same reason the coefficient is:
# so the quantity behind the interval is visible where it is used. P is the joint
# proportion matrix, pr and pc its row and column margins. It agrees with
# psych::cohen.kappa() to the reported precision on all three comparisons.
kappa_ci <- function(a, b, z = 1.96) {
  n <- length(a); k <- cohen_kappa(a, b)
  if (is.na(k)) return(c(NA_real_, NA_real_))
  cats <- sort(base::union(a, b))
  P  <- outer(cats, cats, Vectorize(function(i, j) mean(a == i & b == j)))
  pr <- rowSums(P); pc <- colSums(P); pe <- sum(pr * pc); d <- diag(P)
  A <- sum(d * (1 - (pr + pc) * (1 - k))^2)
  B <- (1 - k)^2 * sum((P * outer(pc, pr, `+`)^2)[row(P) != col(P)])
  C <- (k - pe * (1 - k))^2
  se <- sqrt((A + B - C) / (n * (1 - pe)^2))
  c(k - z * se, k + z * se)
}

band <- function(k) case_when(is.na(k) ~ NA_character_, k >= .81 ~ "almost perfect",
                              k >= .61 ~ "substantial", k >= .41 ~ "moderate",
                              k >= .21 ~ "fair", TRUE ~ "slight")

# ---- Table 15, row 1: author against the blind second coder -----------------
# Joined on idx, so a comment missing from either file drops out rather than
# silently pairing the wrong rows.
pair1 <- three %>%
  transmute(idx, author = align(author_original)) %>%
  inner_join(second %>% transmute(idx, coder2 = align(stance)), by = "idx")

n1  <- nrow(pair1)
ag1 <- sum(pair1$author == pair1$coder2)
k1  <- cohen_kappa(pair1$author, pair1$coder2)

overall <- tibble(
  comparison    = "Author (condition visible) vs. second coder (blind)",
  scheme        = "Five-way", n = n1, n_agree = ag1,
  pct_agreement = round(100 * ag1 / n1, 1),
  kappa         = round(k1, 3),
  ci_low        = round(kappa_ci(pair1$author, pair1$coder2)[1], 3),
  ci_high       = round(kappa_ci(pair1$author, pair1$coder2)[2], 3),
  band          = band(k1)
)
overall
# 47 of 71 comments coded alike, 66.2%, kappa = .515, moderate. The gap is what the
# blind reading costs: the second coder saw only the text, without the condition and
# without the first coder's decision.
write_csv(overall, here("output/tables/study2_comment_overall_agreement.csv"))

# ---- Table 15, rows 2 to 7: the remaining reading pairs ---------------------
# Collapsing to disputed-or-not asks the question the Results actually put, namely
# whether a comment challenges the advertisement's premise, and drops the finer
# distinctions among the ways of not challenging it.
binarise <- function(x) if_else(x == "disputing", "disputing", "not")

rows <- three %>% transmute(idx, author = align(author_original),
                            machine = align(llm_blind), conf = llm_confidence)

# The third reading pair, both blind to the condition and to each other.
pair2 <- three %>%
  transmute(idx, machine = align(llm_blind)) %>%
  inner_join(second %>% transmute(idx, coder2 = align(stance)), by = "idx")
high <- rows %>% filter(conf == "high")

pairwise <- bind_rows(
  tibble(reading_pair = "Author vs. machine pass (blind)", scheme = "Five-way",
         n = nrow(rows),
         agreement_pct = round(100 * mean(rows$author == rows$machine), 1),
         kappa = round(cohen_kappa(rows$author, rows$machine), 3),
         ci_low  = round(kappa_ci(rows$author, rows$machine)[1], 3),
         ci_high = round(kappa_ci(rows$author, rows$machine)[2], 3)),
  tibble(reading_pair = "Author vs. machine pass", scheme = "Disputed or not",
         n = nrow(rows),
         agreement_pct = round(100 * mean(binarise(rows$author) == binarise(rows$machine)), 1),
         kappa = round(cohen_kappa(binarise(rows$author), binarise(rows$machine)), 3),
         ci_low  = round(kappa_ci(binarise(rows$author), binarise(rows$machine))[1], 3),
         ci_high = round(kappa_ci(binarise(rows$author), binarise(rows$machine))[2], 3)),
  # The high-confidence subset is reported without a kappa. On it the disputed-or-not
  # distinction agrees completely and the five-way base rates are too thin for the
  # coefficient to carry meaning.
  tibble(reading_pair = "Author vs. machine pass, high-confidence subset",
         scheme = "Five-way", n = nrow(high),
         agreement_pct = round(100 * mean(high$author == high$machine), 1),
         kappa = NA_real_, ci_low = NA_real_, ci_high = NA_real_),
  # Two rows the table's own title requires, since it announces three readings.
  # The first is the one the Results sentence needs, because the disputed-or-not
  # distinction is what the argument rests on and it has to exist for the two human
  # readings, not for the author against the machine alone. The second closes the
  # table against its title.
  tibble(reading_pair = "Author vs. second coder (blind)", scheme = "Disputed or not",
         n = n1,
         agreement_pct = round(100 * mean(binarise(pair1$author) == binarise(pair1$coder2)), 1),
         kappa = round(cohen_kappa(binarise(pair1$author), binarise(pair1$coder2)), 3),
         ci_low  = round(kappa_ci(binarise(pair1$author), binarise(pair1$coder2))[1], 3),
         ci_high = round(kappa_ci(binarise(pair1$author), binarise(pair1$coder2))[2], 3)),
  tibble(reading_pair = "Second coder vs. machine pass (both blind)", scheme = "Five-way",
         n = nrow(pair2),
         agreement_pct = round(100 * mean(pair2$coder2 == pair2$machine), 1),
         kappa = round(cohen_kappa(pair2$coder2, pair2$machine), 3),
         ci_low  = round(kappa_ci(pair2$coder2, pair2$machine)[1], 3),
         ci_high = round(kappa_ci(pair2$coder2, pair2$machine)[2], 3)),
  tibble(reading_pair = "Second coder vs. machine pass", scheme = "Disputed or not",
         n = nrow(pair2),
         agreement_pct = round(100 * mean(binarise(pair2$coder2) == binarise(pair2$machine)), 1),
         kappa = round(cohen_kappa(binarise(pair2$coder2), binarise(pair2$machine)), 3),
         ci_low  = round(kappa_ci(binarise(pair2$coder2), binarise(pair2$machine))[1], 3),
         ci_high = round(kappa_ci(binarise(pair2$coder2), binarise(pair2$machine))[2], 3))
)

pairwise
# Five-way agreement with the machine is 77.5% (kappa = .663), higher than with the
# human second coder. On the binary disputed-or-not question it reaches 88.7%
# (kappa = .774), and on the 47 comments the model called high-confidence, 95.7%.
# Confidence therefore tracks agreement, which is the check that makes the model's
# reading usable as a third opinion rather than noise.
write_csv(pairwise, here("output/tables/study2_comment_pairwise_agreement.csv"))

# ---- Per-category diagnostic ------------------------------------------------
# A single overall kappa hides which category carries the disagreement. One-vs-rest
# per category shows it, and rare categories are where a low coefficient is expected.
per_cat <- sort(base::union(pair1$author, pair1$coder2)) %>%
  map_dfr(~ {
    a <- pair1$author == .x; b <- pair1$coder2 == .x
    tibble(category = .x, n_coder1 = sum(a), n_coder2 = sum(b),
           pct_agreement = round(100 * mean(a == b), 1),
           kappa_one_vs_rest = round(cohen_kappa(as.character(a), as.character(b)), 3))
  })

per_cat
write_csv(per_cat, here("output/tables/study2_comment_category_agreement.csv"))
