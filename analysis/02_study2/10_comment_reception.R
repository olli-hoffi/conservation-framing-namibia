# =============================================================================
# 02_study2/10_comment_reception.R
#
# WHERE THIS BELONGS
#   In the thesis: Chapter 4, Results: Exploratory Analyses
#   Produces: Figure 8
#   Status: Exploratory. Study 2 has a preregistration, but this script is not
#   part of it. Only 04_confirmatory.R and 05_confirmatory_h2a.R are, and any
#   result here is a finding to be retested rather than a test.
#
# Study 2 - EXPLORATORY reception analysis of the ad comments (NON-preregistered).
# All 71 comments on the 24 WC_ ad Reels were recovered (Graph API + Business Suite
# moderation view; see data/raw/README.md).
#
# At N = 71 (64 with codeable text) this is a small, AUTHOR-CODED qualitative
# CONTENT ANALYSIS, not quantitative NLP (topic models would overfit at this N).
# Each comment was coded by the author on three dimensions in
# MetaAds_comments_coding.csv (joined by source-order idx):
#
#   stance (toward the ad's water-scarcity claim):
#     disputing   - denies/challenges the claim (calls it a lie, counter-facts,
#                   sarcasm implying no scarcity, hostile dismissal)
#     supporting  - agrees with / reinforces the water problem (incl. corroborating
#                   detail or constructive solutions)
#     questioning - asks for source/clarification without a clear position
#     qualifying  - partially agrees; adds nuance (regional variation, the
#                   access-vs-shortage distinction)
#     other       - off-topic, meta (about moderation), or local-language-only
#     noncodeable - empty / emoji- or punctuation-only (no text)
#   language: english / afrikaans / oshiwambo_local / mixed / na
#   local_authority: 1 if the comment invokes lived/local experience
#     ("I'm Namibian", "I live here") to ground its claim, else 0
#
# NOTE on provenance: this script reads the resolved codes. The reported stance codes
# come from three independent readings, one condition-visible, one from an independent
# second coder blind to those codes and to the condition, and one from a language model
# blind to both. Intercoder reliability is computed outside this script. Codes are
# reviewable in the coding CSV and
# should be spot-checked. Reported DESCRIPTIVELY only - no inferential claim.
#
# Inputs:
#   data/raw/MetaAds/MetaAds_comments_2026-07-09.csv   recovered comment text
#   data/raw/MetaAds/MetaAds_comments_coding.csv       author codes (keyed by idx)
# Outputs:
#   output/tables/study2_expl_comment_stance.csv (+ .docx)
#   output/tables/study2_expl_comment_stance_by_condition.csv (+ .docx)
#   output/tables/study2_comment_coded_review.csv   comment records with resolved codes
#   output/figures/fig_study2_comment_stance.{png,pdf}
# =============================================================================

# pacman::p_load() installs a package if missing, then loads it, in one call.
pacman::p_load(here, tidyverse, scales, flextable, officer, viridisLite, patchwork)

# Source every helper in src/ (theme_apa(), condition_colours, save_apa(),
# save_apa_table()). walk() runs source() on each file for its side effect.
list.files(here("src"), full.names = TRUE) %>% walk(source)


# ---- Load + join ------------------------------------------------------------

cond_levels <- c("Neutral", "Empathy", "Social Norm", "Narrative")   # fixed order
# Stance categories in a fixed display order (see header for definitions).
stance_levels <- c("disputing", "other", "qualifying", "questioning",
                   "supporting", "noncodeable")

# Recovered comment text. idx = source-order row number, the key that later joins
# comments to their author codes. recode() folds both "social_norm" and "socialnorm".
comments <- here("data/raw/MetaAds/MetaAds_comments_2026-07-09.csv") %>%
  read_csv(show_col_types = FALSE) %>%
  mutate(idx = row_number(),
         condition = factor(recode(condition,
           neutral = "Neutral", empathy = "Empathy",
           social_norm = "Social Norm", socialnorm = "Social Norm",
           narrative = "Narrative"), levels = cond_levels))

# The author's manual codes (stance / language / local_authority), keyed by idx.
coding <- here("data/raw/MetaAds/MetaAds_comments_coding.csv") %>%
  read_csv(show_col_types = FALSE)

# Join codes onto comments by source order, then make stance an ordered factor.
dat <- comments %>%
  left_join(coding, by = "idx") %>%
  mutate(stance = factor(stance, levels = stance_levels))

# Fail loudly if the join dropped/added rows or left any comment un-coded: all 71
# comments must be present and every stance assigned before proceeding.
stopifnot(nrow(dat) == 71, !anyNA(dat$stance))

# Codeable = comments with interpretable text (drop empty/emoji-only ones).
codeable <- dat %>% filter(stance != "noncodeable")

nrow(dat); nrow(codeable)
# 71 comments recovered, 64 codeable (7 non-codeable: empty / emoji- or
# punctuation-only, no interpretable text).


# ---- Overall stance distribution --------------------------------------------
# Two denominators: pct_all over all 71, pct_codeable over just the text-bearing
# ones (the fairer base for a "% disputing" headline).
# Wilson intervals on the codeable base for the APA completeness
# sweep: at n = 62 the sampling error on a share is wide enough that a bare
# percentage overstates what the coding establishes (APA 7 section 6.44).
wilson_lo <- function(k, n) round(100 * prop.test(k, n, correct = FALSE)$conf.int[1], 1)
wilson_hi <- function(k, n) round(100 * prop.test(k, n, correct = FALSE)$conf.int[2], 1)

stance_overall <- dat %>%
  count(stance) %>%
  mutate(pct_all      = round(100 * n / sum(n), 1),          # share of all 71
         pct_codeable = round(100 * n / nrow(codeable), 1),  # share of codeable only
         ci_low       = map2_dbl(n, nrow(codeable), wilson_lo),
         ci_high      = map2_dbl(n, nrow(codeable), wilson_hi))

# The non-codeable row has no place on the codeable base; blank it rather than
# print an interval around a share of a set it is excluded from.
stance_overall <- stance_overall %>%
  mutate(across(c(pct_codeable, ci_low, ci_high),
                ~ if_else(stance == "noncodeable", NA_real_, .x)))

stance_overall
# Disputing dominates: 40 comments (56.3% of all, 62.5% of codeable). Supporting is
# rare - 4 (6.2% of codeable), tied with questioning; qualifying 6 (9.4%), other 10,
# noncodeable 7. The reception skews sceptical: viewers pushed back on the water-
# scarcity claim far more than they endorsed it.

write_csv(stance_overall, here("output/tables/study2_expl_comment_stance.csv"))

# Headline framed off the codeable base (the fair denominator for "% disputing").
disputing_share  <- round(100 * mean(codeable$stance == "disputing"))
supporting_share <- round(100 * mean(codeable$stance == "supporting"))
c(disputing = disputing_share, supporting = supporting_share)
# 62% of the 64 codeable comments disputed the water-scarcity claim; only 6%
# supported it - a roughly 10:1 sceptical-to-supportive ratio.


# ---- Stance by condition ----------------------------------------------------
# Stance counts within each condition, plus each stance's % of that condition's
# comments (pct is per condition because of the group_by).
stance_by_cond <- dat %>%
  count(condition, stance) %>%
  group_by(condition) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%   # % within condition
  ungroup()

# Condition x stance matrix (pivot wide; empty cells = 0) for a readable overview.
stance_by_cond %>%
  dplyr::select(condition, stance, n) %>%
  pivot_wider(names_from = stance, values_from = n, values_fill = 0)
# Disputing is the modal stance in EVERY condition (Neutral 9, Empathy 9, Social
# Norm 10, Narrative 12), so scepticism is not a framing artefact. The 4 supporting
# comments cluster in Neutral (2) and Social Norm (2); none appear under Empathy or
# Narrative. Too few comments per cell for any inferential read.

write_csv(stance_by_cond, here("output/tables/study2_expl_comment_stance_by_condition.csv"))

# Comment volume per condition (are some framings more provocative?).
vol <- dat %>% count(condition, name = "comments")
vol
# Narrative drew the most comments (23), then Empathy (19), Social Norm (15) and
# Neutral (14). The most-clicked frame is also the most-commented, i.e. the highest-
# engagement frame overall - though the extra comments are mostly disputing.


# ---- Themes: local-authority appeals + language -----------------------------
local_authority_n <- sum(dat$local_authority == 1)
c(n = local_authority_n, pct = round(100 * mean(dat$local_authority == 1)))
# 19 of 71 comments (27%) invoke lived/local experience ("I'm Namibian / I live
# here") to ground their claim - a recurring rhetorical move, most often disputing.

language_dist <- dat %>% count(language)
language_dist
# Overwhelmingly English (57), then na (7, the non-codeable/no-text rows), afrikaans
# (3), oshiwambo_local (3), mixed (1). English dominance justifies author-coding
# without a translation pipeline; the 6 local-language comments are flagged as such.


# ---- Figure: stance distribution by condition (stacked bar) -----------------
# Plotted on the 62 codable comments, which is the set every stance claim in the
# text rests on. Disputing sits at the base of every bar. Stacking the nine
# non-codable comments there instead would start the disputing block at a
# different height in each condition, at 1, 3, 1 and 4, so its share could no
# longer be compared by eye.
# ggplot stacks the FIRST factor level at the top, so disputing is listed last
# to place it at the base of every bar. The legend is re-ordered separately.
stance_order <- c("other", "supporting", "qualifying", "questioning", "disputing")
stance_legend <- c("disputing", "questioning", "qualifying", "supporting", "other")
stance_labs  <- c(disputing = "Disputing", questioning = "Questioning",
                  qualifying = "Qualifying", supporting = "Supporting",
                  other = "Other/Off-Topic")

# Diverging blue-to-red ramp across the four substantive stances (endorse -> dispute),
# plus two clearly distinct greys for the non-substantive categories.
# Viridis over the four ordered stance categories, with the residual in grey.
# A red-blue diverging ramp fails the grayscale test of APA 7 section 7.26,
# because such a ramp is symmetric in lightness by construction, which collapses
# "questioning" and "qualifying" onto the same grey
# (luminance .703 against .737, a gap of .034). Viridis carries .219 between its
# four steps, and the grey sits .130 clear of the nearest of them.
#
# Viridis rather than the mako condition_colours on purpose. Mako marks the four
# framing conditions throughout this thesis, and in this figure the conditions are
# the x axis, so filling by stance in those same colours would give one palette two
# meanings inside one chapter. Viridis is what Figures 3, 4 and 5 already use for
# ordinary categories.
stance_palette <- c(disputing   = viridis(4)[1],   # darkest - opposes the claim
                    questioning = viridis(4)[2],   # neutral ask, leans against
                    qualifying  = viridis(4)[3],   # partial agreement
                    supporting  = viridis(4)[4],   # lightest - endorses the claim
                    other       = "#B8B8B8")       # grey, outside the ordering

# The object is a two-panel figure (APA 7, section 7.26: panels
# labelled A and B at the top left, each panel explained in the general note).
# Panel A pools the codable comments; counts and shares are printed at the bars,
# with no confidence intervals, because the 62 comments are the complete corpus
# the campaign produced (census, not a sample). Panel B is the original
# by-condition stack, unchanged.
p_pool <- codeable %>%
  count(stance) %>%
  mutate(pct = 100 * n / sum(n),
         stance = factor(stance, levels = stance_legend)) %>%
  ggplot(aes(stance, n, fill = stance)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  # Count and share are set on two lines. As a single string the widest label
  # ("37 (59.7%)") overflowed the left panel edge and the two 11.3% labels ran
  # together, because this panel is the narrower of the two. Two lines halve the
  # label width; the added x expansion keeps the outer bars clear of the edge.
  geom_text(aes(label = sprintf("%d\n(%.1f%%)", n, pct)),
            vjust = -0.25, size = 3.7, lineheight = 0.9) +
  scale_fill_manual(values = stance_palette, drop = FALSE) +
  scale_x_discrete(labels = stance_labs, expand = expansion(add = 0.62)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.22))) +
  labs(x = NULL, y = "No. of Comments") +
  theme_apa(base_size = 13) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

p_cond <- codeable %>%
  count(condition, stance) %>%
  # levels in stance_order so the stack runs endorse -> dispute -> grey, not alphabetical.
  mutate(stance = factor(stance, levels = stance_order)) %>%
  ggplot(aes(condition, n, fill = stance)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = stance_palette, labels = stance_labs,
                    breaks = stance_legend, name = "Stance", drop = FALSE) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +   # bars sit exactly on 0
  labs(x = NULL, y = "No. of Comments") +
  theme_apa(base_size = 13, legend_pos = "bottom") +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))   # tilt condition labels

# The legend is collected to the patchwork level and placed below BOTH panels. Left to
# itself patchwork hangs it under panel B alone, where five entries overflow the panel's
# own width and the last one is cut. APA 7.27 wants it below the image in any case.
p_stance <- (p_pool | p_cond) +
  plot_annotation(tag_levels = "A") +
  plot_layout(widths = c(1.15, 1.2), guides = "collect") &
  theme(legend.position = "bottom", legend.box = "horizontal")

p_stance
# APA 7.26 sets an 8-pt floor for text inside a figure, and build.js renders every embedded
# figure into a 6.2-in column. At the 7.0-in save width below the rescale is 0.886, which puts
# the axis text at 9.0 pt, the legend at 9.0 and the bar-count labels at 8.1. The count labels
# need the 3.2 mm size set in the geom_text above to clear the floor.
save_apa(p_stance, "fig_study2_comment_stance", width = apa_width_full + 1.5, height = 4.9)

# Reader-facing coded output: every available text and the resolved codes side by
# side, without usernames or permalinks.
dat %>%
  dplyr::select(idx, round, condition, ad_name, comment_text, stance, language,
         local_authority) %>%
  write_csv(here("output/tables/study2_comment_coded_review.csv"))
