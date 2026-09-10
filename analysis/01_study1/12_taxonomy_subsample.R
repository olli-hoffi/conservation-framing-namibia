# =============================================================================
# 01_study1/12_taxonomy_subsample.R
#
# WHERE THIS BELONGS
#   In the thesis: Chapter 3, Results: the n = 150 subsample
#   Produces: Table 7, Table B1
#   Status: Exploratory. Study 1 carries no preregistration at all, so every
#   association here is descriptive and none of it tests a hypothesis that was
#   stated in advance.
#
# Study 1 (content analysis, PURELY EXPLORATORY, no preregistration): the three
# analyses that needed the Phase B LLM inductive pass, run on the n = 150 sample.
#   (A) image-vs-caption species congruence (do orgs SHOW what they NAME?)
#   (B) visual-quality vs engagement
#   (C) comment stance (what the comment DOES, richer than sentiment polarity)
#
# This script rests on the n = 150 exploratory subsample (stance: 314 comments across
# 38 posts): small-sample, single-coder, hypothesis-GENERATING only. Effect sizes with
# 95% CIs and descriptive language throughout; no test / confirm framing.
#
# INDUCTIVE vs A-PRIORI: the "frames" (f_*) are the deductive codebook, whereas the
# "appeal_*" columns are inductive labels the LLM derived from the data.
#
# Inputs:
#   data/study1/analysis_base.csv              master per-post table (semicolon-delimited)
#   data/study1/features/llm_post_features.csv per-post inductive taxonomy (b2 output)
#   data/study1/features/llm_comment_stance.csv per-comment stance + affect labels
#   data/study1/features/comments_labeled.csv   per-comment VADER score (section D2 only)
# Output: output/tables/tax_*.csv (+ APA .docx)
# =============================================================================

# effsize supplies Cliff's delta; MASS supplies glm.nb() (attempted, then falls back).
pacman::p_load(here, tidyverse, effsize, MASS, flextable, officer)
list.files(here("src"), full.names = TRUE) %>% walk(source)

# set.seed(42) fixes the percentile bootstrap in the Spearman CI helper (Section B).
set.seed(42)

# The 10 text-derivable a-priori frames (f_ prefix) used as the deductive baseline.
frames <- c("Empathy", "Threat", "Efficacy", "Collective_ID", "Normative", "Moral",
            "Economic", "Scientific", "Youth_Addressed", "Interactivity")

# analysis_base = full corpus; delim = ";" because captions contain commas. We do NOT
# filter coded == 1 here: the join to the 150-post LLM sample below is what restricts N.
corpus <- read_delim(here("data/study1/analysis_base.csv"), delim = ";", show_col_types = FALSE) %>%
  mutate(across(paste0("f_", frames), ~ suppressWarnings(as.integer(.))),
         Likes = as.numeric(Likes), Comments = as.numeric(Comments),
         Followers = as.numeric(Followers), eng_rate = as.numeric(eng_rate))

llm_features  <- read_csv(here("data/study1/features/llm_post_features.csv"), show_col_types = FALSE) %>%
  rename(Post_ID = post_id)   # align key name for the join
comment_stance <- read_csv(here("data/study1/features/llm_comment_stance.csv"), show_col_types = FALSE)

# Join the LLM sample to the corpus and add the comment/like discussion ratio.
posts <- llm_features %>%
  left_join(corpus, by = "Post_ID") %>%
  mutate(comment_like_ratio = Comments / (Likes + 1))

c(with_engagement_data = sum(!is.na(posts$eng_rate)), n_posts = nrow(posts))
# All 150 sampled posts carry public engagement data (Likes/Comments/Followers).


# ---- Helpers ----------------------------------------------------------------

# Cliff's delta (present == 1 vs absent == 0) with its CI; NULL if a group is too small
# (< 4) or missing. A robust, distribution-free effect size for skewed engagement.
cliff <- function(y, g) {
  ok <- !is.na(y) & !is.na(g); y <- y[ok]; g <- g[ok]
  if (length(unique(g)) < 2 || min(table(g)) < 4) return(NULL)
  cd <- effsize::cliff.delta(y[g == 1], y[g == 0])
  tibble(delta = as.numeric(cd$estimate), lo = cd$conf.int[1], hi = cd$conf.int[2],
         n_present = sum(g == 1))
}

# Spearman rho + percentile bootstrap 95% CI; returns c(rho, lo, hi). Guards N < 10 with
# NA (too few observations for a stable rho). Used for the ordinal visual-quality ratings.
boot_spearman <- function(x, y, n_resamples = 3000) {
  ok <- !is.na(x) & !is.na(y); x <- x[ok]; y <- y[ok]
  if (length(x) < 10) return(c(NA, NA, NA))
  observed <- suppressWarnings(cor(x, y, method = "spearman"))
  resampled <- replicate(n_resamples, {
    i <- sample(length(x), replace = TRUE)
    suppressWarnings(cor(x[i], y[i], method = "spearman"))
  })
  c(observed, quantile(resampled, .025, na.rm = TRUE), quantile(resampled, .975, na.rm = TRUE))
}


# ===============================================================================
# (A) IMAGE-vs-CAPTION SPECIES CONGRUENCE (show-vs-tell)
# ===============================================================================
# Do orgs SHOW the species they NAME? "match" = shown and named; "names_not_shows" =
# talked about but not pictured; "shows_not_names" = pictured but not named.
congruence <- posts %>% count(species_congruence) %>% mutate(pct = round(100 * n / sum(n)))

congruence
# Of 150 posts: 47% involve no species at all ("neither"), 35% match (show what they
# name), 13% name-not-show, 4% show-not-name, 1% mismatch.

species_posts <- posts %>% filter(species_congruence != "neither")   # posts involving a species
c(n_species_posts   = nrow(species_posts),
  pct_match         = round(100 * mean(species_posts$species_congruence == "match")),
  pct_names_not_show = round(100 * mean(species_posts$species_congruence == "names_not_shows")),
  pct_shows_not_name = round(100 * mean(species_posts$species_congruence == "shows_not_names")))
# Of the 79 posts involving a species, 67% are congruent (show what they name), 24%
# name a species without showing it, 8% show one without naming it -> orgs mostly SHOW
# what they TELL, but a quarter reference species the image does not depict.

write_csv(congruence, here("output/tables/tax_species_congruence.csv"))


# ===============================================================================
# (B) VISUAL QUALITY vs ENGAGEMENT [not reported]
# ===============================================================================
# Ordinal visual-quality ratings (1-3 aesthetic/effort, 0-3 on-image text) correlated
# with engagement rate. Spearman because the ratings are ordinal.
visual_quality <- tibble(quality = c("aesthetic (1-3)", "production effort (1-3)", "on-image text (0-3)"),
                         col     = c("vq_aesthetic_ord", "vq_effort_ord", "vq_text_ord")) %>%
  mutate(s   = map(col, ~ boot_spearman(posts[[.x]], posts$eng_rate)),
         rho = map_dbl(s, 1), lo = map_dbl(s, 2), hi = map_dbl(s, 3)) %>%
  dplyr::select(quality, rho, lo, hi)

visual_quality %>% mutate(across(where(is.numeric), ~ round(., 3)))
# Aesthetic rho = -.19 [-.34, -.04], effort rho = -.21 [-.37, -.06], on-image text
# flat (.02, interval spans zero).
# -> polish does not buy attention. Correlational at n = 150, and image quality does
#    not appear among the reported engagement correlates.

# Does having people in the image relate to engagement? Cliff's delta present vs absent.
people_cliff <- cliff(posts$eng_rate, posts$has_people)
c(delta = round(people_cliff$delta, 3), lo = round(people_cliff$lo, 3), hi = round(people_cliff$hi, 3))
# People present vs absent -> engagement Cliff's delta = -.048 [-.228, .135]: negligible,
# CI spans 0. Showing people neither helps nor hurts engagement at this N.

# Distribution of the aesthetic rating (1 = amateur/screenshot ... 3 = professional).
aesthetic_dist <- posts %>% count(vq_aesthetic_ord) %>% drop_na()
aesthetic_dist
# Aesthetic distribution: 12 amateur (1), 89 competent (2), 49 professional (3) - the
# corpus skews toward competent-to-professional production.


# ===============================================================================
# (C) COMMENT STANCE (true reception beyond sentiment polarity)
# ===============================================================================
# Stance = what the comment DOES (support / criticism / question / off-topic / spam),
# LLM-coded, richer than sentiment polarity. 314 comments across 38 posts.
c(n_comments = nrow(comment_stance), n_posts = n_distinct(comment_stance$post_id))
# 314 comments across 38 posts (the subset of the sample whose comments were coded).

stance_dist <- comment_stance %>% count(stance) %>%
  mutate(pct = round(100 * n / sum(n))) %>% arrange(desc(n))

stance_dist
# Reception is overwhelmingly supportive: support 89% (281), off-topic 5% (17),
# question 3% (8), spam 2% (6), criticism < 1% (2). Genuine criticism is near-absent.

c(pct_criticism = round(100 * mean(comment_stance$stance == "criticism"), 1),
  pct_question  = round(100 * mean(comment_stance$stance == "question"), 1),
  pct_off_spam  = round(100 * mean(comment_stance$stance %in% c("off-topic", "spam")), 1))
# criticism 0.6%, questions 2.5%, off-topic + spam 7.3%.

# Cross-tab of affect x stance: the key nuance is that a "negative" AFFECT comment is
# usually grief / concern in SUPPORT of conservation, not criticism of the org.
stance_by_affect <- comment_stance %>%
  count(affect, stance) %>%
  pivot_wider(names_from = stance, values_from = n, values_fill = 0)

stance_by_affect
# Negative-affect comments are mostly SUPPORT, not criticism: of the negative row, 16 are
# support vs only 2 criticism. The affect polarity and the stance point different ways.

negative_affect <- comment_stance %>% filter(affect == "negative")
c(n_negative = nrow(negative_affect),
  pct_support = round(100 * mean(negative_affect$stance == "support")))
# Of 21 negative-affect comments, 76% are SUPPORT (pro-conservation grief, not criticism)
# -> a sentiment-only "negative" reading would badly misclassify the audience's stance.

write_csv(comment_stance %>% count(stance, affect), here("output/tables/tax_comment_stance.csv"))
save_apa_table(comment_stance %>% count(stance, affect), "tax_comment_stance",
               title = "Table. Comment Stance x Affect (LLM-Coded, 314 Comments / 38 Posts)",
               note  = "Stance = what the comment does; affect = emotional tone. Exploratory.",
               digits = 0)


# ---- (B2) Whose institutions does the sector name? --------------------------

# Namibian community conservation runs through communal conservancies, the institution
# Chapter 2 builds the CBNRM framing on. The place gazetteer in c5_entity_features.py
# cannot answer whether the sector names them, because it holds regions, parks, towns
# and countries and no conservancy at all; a zero there would be an artefact of the
# instrument. This counts the words directly instead. Study1_BlindCoded_184.csv is the
# only shipped file carrying full caption text, so the count runs on those 184 posts
# and the share is reported with a Wilson interval, as elsewhere in this chapter.
#
# captions.csv ships the caption text of all 920 posts, so the count runs on every
# coded post that carries caption text, which is 910 of the 917 coded posts. That is
# the base the chapter reports. The 184-post blind sample is not the base here,
# because it was drawn to estimate agreement rather than prevalence.
caption_base <- corpus %>%
  filter(!is.na(f_Empathy), cap_chars > 0) %>%          # coded posts carrying caption text
  dplyr::select(Post_ID) %>%
  left_join(read_delim(here("data/study1/captions.csv"), delim = ";",
                       show_col_types = FALSE),
            by = "Post_ID") %>%
  mutate(Caption_Text = replace_na(Caption_Text, ""))

stopifnot(nrow(caption_base) == 910)

blind_caps <- caption_base

wilson_ci <- function(k, n, z = 1.96) {              # same interval used for prevalence
  p <- k / n; d <- 1 + z^2 / n
  c(lo = ((p + z^2 / (2 * n)) - z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))) / d,
    hi = ((p + z^2 / (2 * n)) + z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))) / d)
}

inst_terms <- c(conservancy = "conservanc", communal = "communal",
                cbnrm = "cbnrm|community[- ]based", community_generic = "communit")

caption_institutions <- imap_dfr(inst_terms, function(pat, label) {
  k <- sum(str_detect(str_to_lower(blind_caps$Caption_Text), pat))
  ci <- wilson_ci(k, nrow(blind_caps))
  tibble(term = label, n_posts = k, n_total = nrow(blind_caps),
         pct = 100 * k / nrow(blind_caps),
         ci_lo = 100 * ci[["lo"]], ci_hi = 100 * ci[["hi"]])
})

caption_institutions %>% mutate(across(where(is.numeric), ~ round(.x, 1)))
# The generic word carries a quarter of the captions; the named institutions do not.
# Read as a statement about vocabulary, not about what the organisations do offline.

write_csv(caption_institutions, here("output/tables/tax_caption_institutions.csv"))


# ---- (C2) Lexicon sentiment against coded stance [numbers not printed] -------
# Would a lexicon sentiment score have recovered the coded stance?

# The section above uses the LLM's own affect label. This one asks the cheaper question:
# if the study had skipped stance coding and simply scored each comment with a sentiment
# lexicon, would the score have told us what the comment DOES? comments_labeled.csv
# carries one row per retrieved comment in retrieval order, and comment_stance$idx is
# the zero-based position of the coded comment within its post, so the two join on
# post_id plus that position. The stopifnot() guards that join: a silent mismatch would
# pair stance labels with the wrong comments and the cross-tab would be meaningless.
clab_vader <- read_csv(here("data/study1/features/comments_labeled.csv"), show_col_types = FALSE) %>%
  group_by(Post_ID) %>%
  mutate(idx = row_number() - 1L) %>%
  ungroup() %>%
  dplyr::select(post_id = Post_ID, idx, vader)

stance_vader <- comment_stance %>%
  inner_join(clab_vader, by = c("post_id", "idx")) %>%
  mutate(vader_sign = case_when(vader < 0 ~ "negative", vader > 0 ~ "positive", TRUE ~ "neutral"),
         supportive = if_else(stance == "support", "support", "other"))

stopifnot(nrow(stance_vader) == nrow(comment_stance))

sv_tab <- table(sign = stance_vader$vader_sign, stance = stance_vader$supportive)
sv_tab

# Chi-square with Cramer's V as the effect size. NOTE the assumption: one expected count
# is 1.26, below the conventional minimum of 5, so the asymptotic chi-square is only
# approximate here. Fisher's exact test is therefore computed alongside it and carries
# the same verdict, which is what licenses reporting the negligible association.
sv_chisq  <- chisq.test(sv_tab, correct = FALSE)
sv_fisher <- fisher.test(sv_tab)
sv_cramer <- sqrt(as.numeric(sv_chisq$statistic) / (sum(sv_tab) * (min(dim(sv_tab)) - 1)))

round(sv_chisq$expected, 2)   # inspect the expected counts behind that caveat

# Percentile bootstrap over comments for the Cramer's V interval, so the effect size is
# reported with its precision rather than as a bare point estimate (seed fixed above).
sv_boot <- replicate(3000, {
  i  <- sample(nrow(stance_vader), replace = TRUE)
  tb <- table(stance_vader$vader_sign[i], stance_vader$supportive[i])
  if (min(dim(tb)) < 2) NA_real_ else {
    ct <- suppressWarnings(chisq.test(tb, correct = FALSE))
    sqrt(as.numeric(ct$statistic) / (sum(tb) * (min(dim(tb)) - 1)))
  }
})

stance_vader_assoc <- tibble(
  n_comments  = sum(sv_tab),
  v_lo        = unname(quantile(sv_boot, .025, na.rm = TRUE)),
  v_hi        = unname(quantile(sv_boot, .975, na.rm = TRUE)),
  chisq       = as.numeric(sv_chisq$statistic),
  df          = as.integer(sv_chisq$parameter),
  p_chisq     = as.numeric(sv_chisq$p.value),
  p_fisher    = as.numeric(sv_fisher$p.value),
  cramers_v   = sv_cramer,
  min_expected = min(sv_chisq$expected))

stance_vader_assoc %>% mutate(across(where(is.numeric), ~ round(.x, 3)))
# One expected cell is 1.26, so the chi-square is approximate. Fisher agrees and
# the association is negligible.
# -> automated sentiment is no substitute for reading the comments, which is why
#    stance was coded by hand. Stated in words and evidenced by the nine-of-twelve
#    counter-check.

# The counter-check that makes the point concretely: of the comments the lexicon scored
# NEGATIVE, how many were coded supportive, and how did the coded criticism score?
neg_scored <- stance_vader %>% filter(vader < 0)
c(n_scored_negative = nrow(neg_scored),
  n_coded_support   = sum(neg_scored$stance == "support"),
  n_coded_criticism = sum(neg_scored$stance == "criticism"))

# The chapter quotes an interval on the support share among the negatively scored
# comments, so it is computed here rather than by hand. wilson_ci() is the same
# uncorrected Wilson helper the rest of this chapter uses. The base is small, which
# the width shows, and the point of reporting it is exactly that.
neg_support_ci <- {
  k <- sum(neg_scored$stance == "support"); n <- nrow(neg_scored)
  ci <- wilson_ci(k, n)
  tibble(quantity = "Coded supportive among lexicon-negative comments",
         k = k, n = n, pct = round(100 * k / n, 1),
         ci_lo = round(100 * ci[["lo"]], 1), ci_hi = round(100 * ci[["hi"]], 1))
}

neg_support_ci
write_csv(neg_support_ci, here("output/tables/tax_negative_support_ci.csv"))
stance_vader %>% filter(stance == "criticism") %>% dplyr::select(post_id, idx, vader, affect)

write_csv(stance_vader_assoc, here("output/tables/tax_stance_vader_assoc.csv"))

# Question-rate by post frame: share of a frame's comments that are questions (audience
# asking rather than praising), when the frame is present vs absent.
stance_by_frame <- comment_stance %>%
  left_join(corpus %>% dplyr::select(Post_ID, f_Threat, f_Empathy, f_Interactivity),
            by = c("post_id" = "Post_ID"))

# ---- Question rate by frame [not reported] -----------------------------------
# Are comments on posts carrying a frame more often questions?
question_rate_by_frame <- map_dfr(c("f_Threat", "f_Empathy", "f_Interactivity"), function(f) {
  g <- stance_by_frame[[f]]; ok <- !is.na(g)
  tibble(frame     = sub("f_", "", f),
         q_present = round(mean(stance_by_frame$stance[ok & g == 1] == "question"), 3),
         q_absent  = round(mean(stance_by_frame$stance[ok & g == 0] == "question"), 3))
})

question_rate_by_frame
# Rates stay at or below 4%. Empathy posts draw more when present (.037 against
# .013), interactivity posts draw none.
# -> the audience affirms rather than asks, whatever the frame. The overall
#    question share of 2.5% is reported, without a split by frame.


# -----------------------------------------------------------------------------
# The Results paragraph "The Register and Stance of the Comments" reports three
# numbers that had no committed computation: the concentration of the coded posts
# on the most-discussed posts (Cliff's delta), and the naive against the
# cluster-adjusted 95% CI around the support share. Both are recomputed here so
# that the paragraph reproduces from the package.
#
# Cliff's delta uses n_comments_scraped, the comments actually retrieved, not the
# platform-reported Comments column: the claim concerns where the CODED comments
# sit, and those come from the retrieved set. The comparison group is the
# remaining comment-bearing posts, so posts with no comments at all cannot
# inflate the separation.
# =============================================================================

coded_ids <- comment_stance %>% distinct(post_id) %>% pull(post_id)

conc <- corpus %>%
  mutate(grp = if_else(Post_ID %in% coded_ids, "coded", "rest")) %>%
  filter(grp == "coded" | Comments > 0) %>%
  filter(!is.na(n_comments_scraped))

cliff_conc <- cliff.delta(
  conc %>% filter(grp == "coded") %>% pull(n_comments_scraped),
  conc %>% filter(grp == "rest")  %>% pull(n_comments_scraped)
)

# Support share, naive Wilson interval against a cluster bootstrap that resamples
# POSTS, not comments. Comments are not independent within a post, so the naive
# interval is too narrow; the bootstrap carries the dependence.
sup <- comment_stance %>%
  group_by(post_id) %>%
  summarise(k = sum(stance == "support"), n = n(), .groups = "drop")

p_hat  <- sum(sup$k) / sum(sup$n)
wil    <- prop.test(sum(sup$k), sum(sup$n), correct = FALSE)$conf.int

set.seed(2026)
boot_p <- replicate(10000, {
  idx <- sample(nrow(sup), nrow(sup), replace = TRUE)
  sum(sup$k[idx]) / sum(sup$n[idx])
})
clus <- quantile(boot_p, c(.025, .975))

# The negative-affect share is quoted in the same Results paragraph and had no
# interval; a Wilson interval is what the surrounding shares already carry.
neg_aff  <- comment_stance %>% filter(affect == "negative")
neg_sup  <- sum(neg_aff$stance == "support")
neg_wil  <- prop.test(neg_sup, nrow(neg_aff), correct = FALSE)$conf.int

reception_bounds <- tibble(
  quantity = c("cliff_delta_concentration", "cliff_delta_ci_low", "cliff_delta_ci_high",
               "support_share",
               "ci_naive_low", "ci_naive_high",
               "ci_cluster_low", "ci_cluster_high",
               "neg_affect_support_share", "neg_affect_ci_low", "neg_affect_ci_high"),
  value    = c(cliff_conc$estimate, cliff_conc$conf.int[1], cliff_conc$conf.int[2],
               p_hat * 100,
               wil[1] * 100, wil[2] * 100,
               clus[1] * 100, clus[2] * 100,
               100 * neg_sup / nrow(neg_aff), neg_wil[1] * 100, neg_wil[2] * 100),
  note     = c(rep(paste0("coded posts (n = ", sum(conc$grp == "coded"), ") vs remaining ",
                      "comment-bearing posts (n = ", sum(conc$grp == "rest"), "), on n_comments_scraped"), 3),
               paste0(sum(sup$k), " of ", sum(sup$n), " comments"),
               "Wilson, comments treated as independent",
               "Wilson, comments treated as independent",
               "cluster bootstrap over 38 posts, 10,000 draws, seed 2026",
               "cluster bootstrap over 38 posts, 10,000 draws, seed 2026",
               rep(paste0(neg_sup, " of ", nrow(neg_aff), " negative-affect comments coded support"), 3))
)

write_csv(reception_bounds, here("output/tables/tax_reception_bounds.csv"))
print(reception_bounds, n = Inf)

# =============================================================================
# Retrieval accounting
# -----------------------------------------------------------------------------
# The Method reports how much of the platform-reported comment volume the scrape
# actually retrieved, and how much the 15-comment cap costs on the posts it binds.
# n_comments_scraped is the retrieved count, Comments the platform-reported one.
# A post counts as capped when the scrape returned at least 15 comments, which is
# where the collector stops.
# =============================================================================

retrieval <- corpus %>%
  filter(Comments > 0, !is.na(n_comments_scraped)) %>%
  mutate(capped = n_comments_scraped >= 15)

retrieval_rates <- bind_rows(
  retrieval %>%
    summarise(scope = "all comment-bearing posts",
              n_posts = n(),
              retrieved = sum(n_comments_scraped),
              reported = sum(Comments)),
  # The coded subsample, the posts the stance coding actually ran on. Chapter 3 reports
  # its retrieval rate against the corpus-wide one to bound how much of the discussion
  # the coded comments represent. Without this row the reported figure had no source.
  retrieval %>%
    filter(Post_ID %in% coded_ids) %>%
    summarise(scope = "coded posts (stance subsample)",
              n_posts = n(),
              retrieved = sum(n_comments_scraped),
              reported = sum(Comments)),
  retrieval %>%
    filter(capped) %>%
    summarise(scope = "capped posts (>= 15 retrieved)",
              n_posts = n(),
              retrieved = sum(n_comments_scraped),
              reported = sum(Comments))
) %>%
  mutate(retrieval_pct = round(100 * retrieved / reported, 1))

write_csv(retrieval_rates, here("output/tables/tax_retrieval_rates.csv"))
print(retrieval_rates, n = Inf)


# =============================================================================
# Subsample shares with intervals (Table 4)
# -----------------------------------------------------------------------------
# The appeal-type shares, the explicit-ask share and the species-congruence
# shares all reach Table 4 as bare percentages on n = 150 or n = 73, where the
# sampling error is large enough to matter. APA 7 section 6.44 asks for an
# interval wherever one can be formed. Wilson intervals, as elsewhere in this
# chapter. Nothing about the point estimates changes.
# =============================================================================

wilson_share <- function(label, k, n) {
  ci <- prop.test(k, n, correct = FALSE)$conf.int
  tibble(quantity = label, k = k, n = n,
         pct = round(100 * k / n, 1),
         lo  = round(100 * ci[1], 1),
         hi  = round(100 * ci[2], 1))
}

appeal_labels <- c(appeal_emotional = "Emotional appeal",
                   appeal_identity = "Identity appeal",
                   appeal_informational = "Informational appeal",
                   appeal_aesthetic = "Aesthetic appeal",
                   appeal_social_normative = "Social-normative appeal",
                   appeal_economic = "Economic appeal")

# Species congruence is defined only on the posts whose caption names a species,
# so the three naming categories form the denominator and "neither" is excluded.
named_species <- llm_features %>%
  filter(species_congruence %in% c("match", "mismatch", "names_not_shows"))

subsample_shares <- bind_rows(
  imap_dfr(appeal_labels, ~ wilson_share(.x, sum(llm_features[[.y]] == 1, na.rm = TRUE),
                                         nrow(llm_features))),
  wilson_share("Carries an explicit ask",
               sum(llm_features$cta_present == 1, na.rm = TRUE), nrow(llm_features)),
  wilson_share("Shows the species it names",
               sum(named_species$species_congruence == "match"), nrow(named_species)),
  wilson_share("Names a species the cover does not show",
               sum(named_species$species_congruence == "names_not_shows"), nrow(named_species))
)

subsample_shares
write_csv(subsample_shares, here("output/tables/tax_subsample_shares.csv"))
# [not reported, partial] "Shows the species it names" and "Carries an explicit ask"
# carry into Table 4. The six appeal-type rows and "Names a species the cover does
# not show" do not.


# ---- Narrative structure of the n = 150 subsample ---------------------------
# The Results text reports these counts ("anecdote (34 posts) and thematic display
# (31)") and the Brief Discussion turns on the third ("Only five of the 150
# subsample posts carried a complete problem-solution arc"), so the tabulation is
# written out here rather than read off a console.
narrative_counts <- llm_features %>%
  filter(!is.na(narrative_structure), narrative_structure != "") %>%
  count(narrative_structure, sort = TRUE, name = "n") %>%
  mutate(pct = round(100 * n / sum(n), 1))

narrative_counts
# anecdote (34) and showcase (31) lead. Only 5 of the 150 posts carry a complete
# problem-solution arc, which is the absence the Brief Discussion reports.
write_csv(narrative_counts, here("output/tables/tax_narrative_structure.csv"))


# ---- Table B1: how the drawn sample covers each stratification axis ---------
# The draw was stratified, so the appendix reports whether every level of every
# axis actually appears in the 150 posts and how unevenly they are filled. A
# stratum that lost a level would limit what the subsample can say.
draw <- read_csv(here("data/study1/sample_150.csv"), show_col_types = FALSE)

cover <- function(col, label) {
  v <- draw[[col]]; v <- v[!is.na(v) & v != ""]
  tb <- table(v)
  tibble(stratum = label, levels = length(tb),
         min_cell = min(tb), max_cell = max(tb),
         coverage = paste0(length(tb), " of ", length(tb), " present"))
}
sample_coverage <- bind_rows(
  cover("functional_genre", "Functional genre"),
  cover("Post_Format",      "Post format"),
  cover("Actor_Name",       "Organization"),
  cover("visual_zeroshot",  "Visual genre (zero-shot)"),
  # The three frame-space terciles are separate binary axes, so they are counted
  # together as one stratum with three dimensions.
  tibble(stratum = "Frame-space tercile", levels = 3L,
         min_cell = min(c(table(draw$pca1_bin), table(draw$pca2_bin), table(draw$pca3_bin))),
         max_cell = max(c(table(draw$pca1_bin), table(draw$pca2_bin), table(draw$pca3_bin))),
         coverage = "all terciles populated on all three dimensions"),
  # Keyword-flagged content types are overlapping indicators, not a partition,
  # so each is counted on its own.
  tibble(stratum = "New content types (keyword)", levels = 4L,
         min_cell = min(sum(draw$commemoration_day, na.rm = TRUE), sum(draw$live_series, na.rm = TRUE),
                        sum(draw$hiring, na.rm = TRUE), sum(draw$pure_info, na.rm = TRUE)),
         max_cell = max(sum(draw$commemoration_day, na.rm = TRUE), sum(draw$live_series, na.rm = TRUE),
                        sum(draw$hiring, na.rm = TRUE), sum(draw$pure_info, na.rm = TRUE)),
         coverage = paste0("commemoration ", sum(draw$commemoration_day, na.rm = TRUE),
                           ", live/broadcast ", sum(draw$live_series, na.rm = TRUE),
                           ", recruitment ", sum(draw$hiring, na.rm = TRUE),
                           ", pure information ", sum(draw$pure_info, na.rm = TRUE))),
  tibble(stratum = "Comment-bearing posts (>= 3 scraped comments)", levels = NA_integer_,
         min_cell = NA_integer_, max_cell = NA_integer_,
         coverage = as.character(sum(draw$n_comments_scraped >= 3, na.rm = TRUE))),
  tibble(stratum = "High-discussion posts (>= 20 total comments)", levels = NA_integer_,
         min_cell = NA_integer_, max_cell = NA_integer_,
         coverage = as.character(sum(draw$Comments >= 20, na.rm = TRUE))),
  tibble(stratum = "Codebook blind-spot posts", levels = NA_integer_,
         min_cell = NA_integer_, max_cell = NA_integer_,
         coverage = as.character(sum(draw$blindspot == 1 | draw$blindspot == TRUE, na.rm = TRUE)))
)

sample_coverage
# Every level of every stratified axis appears in the draw. The unevenness is
# real but bounded: genres run 6 to 24 posts, organizations 1 to 17. No axis
# lost a level, so the subsample can speak to each of them.
write_csv(sample_coverage, here("output/tables/tax_sample_coverage.csv"))
