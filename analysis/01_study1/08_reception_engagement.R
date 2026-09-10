# =============================================================================
# 01_study1/08_reception_engagement.R
#
# WHERE THIS BELONGS
#   In the thesis: Chapter 3, Results: Reception and Engagement
#   Produces: Table 5; the Cliff's delta column of Table 3; two rows of Table 6
#   Status: Exploratory. Study 1 carries no preregistration at all, so every
#   association here is descriptive and none of it tests a hypothesis that was
#   stated in advance.
#
# Study 1 (content analysis, PURELY EXPLORATORY, no preregistration):
# engagement & reception (Theme 4). What the audience DOES: the comment/like
# discussion ratio per frame, an over-dispersed count model for comments, comment
# topics, and the commenter / shared-audience network.
#
# Everything is exploratory and descriptive; associations are CORRELATIONAL, with
# no causal reading. Engagement is public-only (likes + comments; reach / saves /
# shares are unobtainable from the Graph API).
#
# Inputs : data/study1/analysis_base.csv (master per-post table, semicolon-delimited)
#          data/study1/features/comments_labeled.csv, comment_topics_info.csv,
#          commenter_summary.csv, org_shared_audience_edges.csv,
#          org_feature_matrix.csv
# Output : output/tables/eng_frame_discussion_cliff.csv, eng_comments_nb_theta.csv,
#          eng_comments_nb_irr.csv (+ APA .docx), eng_comment_topic_rollup.csv,
#          eng_comment_concentration.csv, eng_audience_overlap.csv,
#          eng_audience_network_summary.csv
# =============================================================================

# MASS supplies glm.nb() (negative-binomial GLM); igraph the shared-audience network;
# effsize the Cliff's delta effect size.
pacman::p_load(here, tidyverse, MASS, igraph, effsize, flextable, officer)
list.files(here("src"), full.names = TRUE) %>% walk(source)
# MASS also exports select(); it masks dplyr::select, so call dplyr::select explicitly
# throughout to keep the tidyverse pipes working.

set.seed(42)   # fixes the random stream for the whole script.
# The network layout reseeds itself locally.

frames <- c("Empathy", "Threat", "Efficacy", "Collective_ID", "Normative", "Moral",
            "Economic", "Scientific", "Youth_Addressed", "Interactivity",
            "Youth_Style", "Protagonist", "Onscreen_Text", "Data_Visual")

# Coded posts, with frame flags as 0/1 and the engagement counts coerced to numeric.
# delim = ";" because captions contain commas.
base <- read_delim(here("data/study1/analysis_base.csv"), delim = ";", show_col_types = FALSE) %>%
  filter(coded == 1) %>%
  mutate(across(paste0("f_", frames), ~ suppressWarnings(as.integer(.))),
         Likes = as.numeric(Likes), Comments = as.numeric(Comments),
         Followers = as.numeric(Followers), eng_rate = as.numeric(eng_rate))

# Committed feature snapshots the Python pipeline produced (see folder README).
clab  <- read_csv(here("data/study1/features/comments_labeled.csv"), show_col_types = FALSE) %>%
  filter(in_corpus == 1)   # individual comments + VADER sentiment + topic; in-corpus only
tinfo <- read_csv(here("data/study1/features/comment_topics_info.csv"), show_col_types = FALSE)   # BERTopic topic table
csum  <- read_csv(here("data/study1/features/commenter_summary.csv"), show_col_types = FALSE)     # per-commenter activity
shar  <- read_csv(here("data/study1/features/org_shared_audience_edges.csv"), show_col_types = FALSE)   # org-org shared-commenter edges

tibble(n_coded = nrow(base), comments_joined = nrow(clab),
       posts_with_comments = n_distinct(clab$Post_ID))
# 917 coded posts; 2,172 in-corpus comments joined over 438 posts (the committed
# comment snapshot covers the posts that drew public comments).


# ---- 1. Engagement decomposition (approval vs deliberation) -----------------
# comment/like ratio = Comments / (Likes + 1): a proxy for DELIBERATION (people
# writing) vs passive APPROVAL (people liking). +1 avoids divide-by-zero on no-like posts.
# Drop the 4 posts with Likes = -1 (a hidden-likes sentinel from the Graph API): a
# like-based ratio is undefined (0/0 = NaN) when the account has hidden its like count.
d <- base %>% filter(Likes >= 0) %>%
  mutate(comment_like_ratio = Comments / (Likes + 1))

# Which frames raise the discussion ratio? Cliff's delta present vs absent + 95% CI.
# Cliff's delta = a non-parametric effect size in [-1, 1]: the probability a
# frame-present post out-ranks a frame-absent post on the discussion ratio, minus the
# reverse. Robust to the heavy skew; assumes no distribution.
cliff_tab <- map_dfr(frames, function(f) {
  g <- d[[paste0("f_", f)]]; y <- d$comment_like_ratio
  ok <- !is.na(g) & !is.na(y); g <- g[ok]; y <- y[ok]
  if (length(unique(g)) < 2 || min(table(g)) < 5) return(NULL)   # need both groups with >= 5 posts, else skip
  cd <- effsize::cliff.delta(y[g == 1], y[g == 0])               # present vs absent
  tibble(frame = f, n_present = sum(g == 1), delta = as.numeric(cd$estimate),
         lo = cd$conf.int[1], hi = cd$conf.int[2])
}) %>% arrange(desc(delta))

cliff_tab %>% mutate(across(where(is.numeric), ~ round(., 3)))
# Empathy has the largest reliable positive delta (.34 [.25, .42]), then Threat (.26
# [.17, .34]) and Protagonist (.22 [.14, .29]): emotional/character-driven posts
# out-rank others on discussion. Economic (-.22) and Moral (-.17) sit reliably below
# zero. Frames that carry affect and a protagonist drive the most discussion per like.

write_csv(cliff_tab, here("output/tables/eng_frame_discussion_cliff.csv"))


# ---- 2. Negative-binomial count model for comments --------------------------
# Comments are over-dispersed counts (variance >> mean), so a negative-binomial GLM
# fits better than Poisson. log(Followers) enters as an exposure-style control (a
# bigger audience mechanically yields more comments). Post_Format releveled to Image.
dm <- base %>%
  filter(is.finite(Followers), Followers > 0,
         !is.na(f_Empathy), !is.na(f_Threat), !is.na(f_Scientific),
         !is.na(f_Protagonist), !is.na(f_Interactivity)) %>%
  mutate(Post_Format = relevel(factor(Post_Format), ref = "Image"),
         logF = log(Followers))   # log-followers control (exposure)

nrow(dm)
# 881 posts enter the comment model (positive followers + complete frame flags).

# tryCatch so a non-convergence returns NULL instead of crashing the script.
nb <- tryCatch(glm.nb(Comments ~ Post_Format + f_Empathy + f_Threat + f_Efficacy +
                        f_Scientific + f_Protagonist + f_Interactivity + logF, data = dm),
               error = function(e) NULL)

ci <- suppressMessages(confint(nb))   # profile-likelihood 95% CIs on the log scale
# exp(coef) turns the log-counts into INCIDENCE RATE RATIOS: IRR > 1 = that feature
# multiplies the expected comment count upward, holding the others fixed.
irr <- as.data.frame(cbind(IRR = exp(coef(nb)), exp(ci))) %>%
  rownames_to_column("term") %>%
  filter(term != "(Intercept)") %>%
  rename(ci_lo = `2.5 %`, ci_hi = `97.5 %`)

irr %>% mutate(across(where(is.numeric), ~ round(., 3)))
# log(Followers) IRR = 2.16 [1.97, 2.37] (bigger audience -> more comments, as expected).
# f_Interactivity IRR = 3.94 [2.02, 8.70], Post_FormatVideo 3.23 [2.39, 4.39],
# f_Empathy 1.96 [1.40, 2.76] and f_Threat 1.91 [1.37, 2.67] multiply the comment
# count upward; f_Scientific (0.97) and f_Efficacy (1.18) have CIs spanning 1 (ns).
# Interactive, video, and emotional posts draw markedly more comments than Image.

# NB theta: a small theta = strong overdispersion, so NB is preferred over Poisson.
nb$theta
# theta = 0.58 (low): comments are strongly over-dispersed, confirming NB over Poisson.
# Written out, because the chapter reports theta and a script comment is not a source.
tibble(quantity = "nb_theta", value = nb$theta, se = nb$SE.theta, n_posts = nrow(dm)) %>%
  write_csv(here("output", "tables", "eng_comments_nb_theta.csv"))

write_csv(irr, here("output/tables/eng_comments_nb_irr.csv"))
save_apa_table(irr, "eng_comments_nb_irr",
               title = "Table. Negative-Binomial Model: Comment Count by Format and Frame",
               note  = "IRR = incidence rate ratio (exp of the coefficient); IRR > 1 = more comments. log(Followers) = exposure control.",
               digits = 3)


# ---- 3. Comment topics -------------------------------------------------------
# Top BERTopic comment topics (topic == -1 is the noise/outlier bucket, so keep
# topic >= 0) with their most representative words.
tinfo %>% filter(topic >= 0) %>% slice_max(count, n = 8) %>%
  dplyr::select(topic, count, top_words)
# The largest comment topics are short affective / appreciation clusters (topic 0:
# "beautiful, love"; topic 1: "amazing beautiful, thanks"; topic 4: "great work, good
# job") rather than substantive debate. The audience mostly praises, seldom argues.

# The Results prose reports three rolled-up comment-topic figures. The rollup is a reading of
# the BERTopic output rather than an output of it, so the topic ids are named here
# instead of being inferred later: topics 0, 1 and 2 are the emoji and short-praise
# clusters, topic 3 is the largest elaborated topic (volunteering testimony), and
# topic 24 is the intention-to-act topic (asking how to sign up).
topic_rollup <- tibble(
  grouping = c("Emoji and short praise", "Largest elaborated topic", "Intention to act"),
  topics   = c("0, 1, 2", "3", "24"),
  comments = c(sum(tinfo$count[tinfo$topic %in% c(0, 1, 2)]),
               sum(tinfo$count[tinfo$topic == 3]),
               sum(tinfo$count[tinfo$topic == 24]))
)

write_csv(topic_rollup, here("output/tables/eng_comment_topic_rollup.csv"))
topic_rollup


# ---- 4. Commenter + shared-audience network ---------------------------------
# How much the commenter base overlaps: repeat commenters and cross-org commenters,
# then a network where an edge = two orgs sharing commenters (audience overlap).
commenter_overlap <- tibble(
  unique_commenters = nrow(csum),
  repeat_commenters = sum(csum$n_comments >= 2),
  repeat_pct        = round(100 * mean(csum$n_comments >= 2)),
  cross_org         = sum(csum$n_orgs >= 2)
)

commenter_overlap
# 1,225 unique commenters; 25% comment more than once; 24 comment across >= 2 orgs.
# 24 out of 1,225 is not partial overlap but near-complete separation: section 5
# below counts how far a commenter's activity reaches across organisations.

# The Results text reports how far the retrieved comment corpus concentrates on a
# single account ("That account supplied 57 of the 62 posts with at least 20
# comments and 41.1% of the 2,172 retrieved comments"). Both figures bound the
# reception estimate, so they are computed here rather than by hand.
high_comment_posts <- base %>% filter(Comments >= 20)

comments_by_org <- clab %>%
  left_join(base %>% dplyr::select(Post_ID, Actor_Name), by = "Post_ID") %>%
  count(Actor_Name, name = "comments")

top_org <- comments_by_org %>% slice_max(comments, n = 1)

comment_concentration <- tibble(
  organisation          = top_org$Actor_Name,
  posts_ge_20_comments  = nrow(high_comment_posts),
  of_which_this_org     = sum(high_comment_posts$Actor_Name == top_org$Actor_Name),
  comments_retrieved    = nrow(clab),
  comments_this_org     = top_org$comments,
  comments_this_org_pct = round(100 * top_org$comments / nrow(clab), 1)
)

write_csv(comment_concentration, here("output/tables/eng_comment_concentration.csv"))
comment_concentration


# ---- 5. How far does a commenter's activity reach across organisations? -----
# Hold the commenter-by-post incidence fixed and count, per commenter, how many
# distinct organisations they touched. A commenter who left 40 comments on one
# account still counts once per organisation, so volume cannot inflate reach.
inc_ca   <- clab %>% filter(!is.na(commenter), commenter != "",
                            !is.na(src_org), src_org != "") %>% distinct(commenter, Post_ID)
post_org <- clab %>% filter(!is.na(src_org), src_org != "") %>% distinct(Post_ID, src_org)

# Commenters with >= 2 posts are the only ones who COULD cross, so the "stayed inside
# one organisation" share is computed on them alone.
elig_ca <- inc_ca %>% count(commenter) %>% filter(n >= 2) %>% pull(commenter)

reach <- inc_ca %>%
  left_join(post_org, by = "Post_ID") %>%
  group_by(commenter) %>%
  summarise(k = n_distinct(src_org), .groups = "drop")

audience_overlap <- tibble(
  eligible_commenters = length(elig_ca),
  obs_multi_org       = sum(reach$k >= 2),
  obs_stay_pct        = round(100 * mean(reach$k[reach$commenter %in% elig_ca] == 1), 1)
)

audience_overlap
# 24 of the 301 commenters with >= 2 posts reached a second organisation; 92.0% stayed
# inside one. The retrieval cap under-observes overlap, so the bias runs AGAINST this
# finding rather than producing it.
write_csv(audience_overlap, here("output/tables/eng_audience_overlap.csv"))

# graph_from_data_frame() builds the org-org graph; weight = number of shared commenters.
ga <- graph_from_data_frame(shar %>% rename(from = org_a, to = org_b) %>%
                              mutate(weight = shared_commenters), directed = FALSE)

network_summary <- tibble(
  orgs          = gorder(ga),   # number of vertices (orgs)
  edges         = gsize(ga),    # number of edges (org pairs sharing >= 1 commenter)
  densest_pair  = paste(shar$org_a[1], "-", shar$org_b[1]),
  densest_shared = shar$shared_commenters[1]
)

network_summary
# 13 orgs, 19 audience-overlap edges; the densest pair (ccfcheetah - giraffe_conservation)
# shares 5 commenters. Conservation orgs partly share one Namibian audience.
# Table 6 quotes the edge count and the Results prose the densest pair, and a
# reported number cannot live in a script comment, so they are written out.
write_csv(network_summary, here("output/tables/eng_audience_network_summary.csv"))
