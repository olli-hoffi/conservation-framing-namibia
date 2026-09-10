# =============================================================================
# 01_study1/13_reception_comment_form.R
#
# WHERE THIS BELONGS
#   In the thesis: Chapter 3, Results: Reception and Engagement
#   Produces: Table 6
#   Status: Exploratory. Study 1 carries no preregistration at all, so every
#   association here is descriptive and none of it tests a hypothesis that was
#   stated in advance.
#
# What the comments look like, rather than what they say.
#
# The reception finding of Chapter 3 is that the dominant response is minimal
# affirmation. That claim rests on the FORM of the comments, so this script
# measures form: how many are emoji only, how short they are, how many ask a
# question, and how much of the volume comes from returning commenters.
#
# Every share carries a Wilson interval. On proportions this far from .5, and with
# some cells small, the normal approximation would put bounds outside 0 and 1.
#
# Status: exploratory, descriptive. Study 1 is not preregistered.
# Runtime: about 2 seconds.
#
# in:  data/study1/comments_text.csv
#      data/study1/features/comments_labeled.csv
#      data/study1/features/comment_topics_info.csv
#      data/study1/analysis_base.csv
# out: output/tables/eng_comment_form.csv
#      output/tables/eng_comment_topics_by_cta.csv
#      output/tables/eng_comment_cta_summary.csv
#
# Feeds the comment-form rows of Table 6.
# =============================================================================

pacman::p_load(here, tidyverse)
list.files(here("src"), full.names = TRUE) %>% walk(source)

cmt <- read_csv(here("data/study1/comments_text.csv"), show_col_types = FALSE) %>%
  mutate(text = replace_na(text, ""), commenter = replace_na(commenter, ""))

n_all  <- nrow(cmt)                       # every retrieved comment, text or not
texted <- cmt %>% filter(str_trim(text) != "")   # the ones carrying any text

# The emoji ranges cover the pictographic blocks plus the joiners, variation
# selectors and general punctuation that sit between them in a sequence. Stripping
# all of that and finding nothing left is what "emoji only" means here.
# ---- 1. What counts as emoji, and how a word is counted ---------------------
EMOJI <- "[\U0001F000-\U0001FAFF\U00002600-\U000027BF\U0001F1E6-\U0001F1FF\U00002190-\U000021FF\U00002B00-\U00002BFF\U0000FE00-\U0000FE0F\U0001F900-\U0001F9FF\U0000200D\U00002000-\U0000206F\U00002100-\U0000214F]+"

# The two use different substitutions on purpose. Emoji-only asks whether anything
# remains once the emoji are DELETED. Word counting REPLACES them with a space, so
# that "word<emoji>word" counts as two words rather than running them together.
is_emoji_only <- function(t) str_trim(t) != "" & str_trim(str_remove_all(t, EMOJI)) == ""
word_count    <- function(t) str_count(str_squish(str_replace_all(t, EMOJI, " ")), "\\S+")

# Wilson interval rather than Wald: it stays inside 0 and 1 and behaves at the
# extremes, which matters here because several shares sit near 5% or above 55%.

# ---- 2. Wilson interval, used for every share below -------------------------
wilson <- function(k, n, z = 1.96) {
  if (n == 0) return(c(NA_real_, NA_real_))
  p <- k / n; den <- 1 + z^2 / n
  centre <- (p + z^2 / (2 * n)) / den
  half   <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / den
  100 * c(centre - half, centre + half)
}
row <- function(label, k, n, note = "") {
  ci <- wilson(k, n)
  tibble(quantity = label, k = k, n = n, pct = round(100 * k / n, 1),
         ci_low = round(ci[1], 1), ci_high = round(ci[2], 1), note = note)
}


# ---- 3. The form measures themselves -----------------------------------------
wc <- word_count(texted$text)

# Commenter identities are pseudonyms, so "repeat" means the same pseudonym, not a
# guess from the text. Comments without an identity are excluded from these two rows.
named  <- texted %>% filter(commenter != "")
counts <- named %>% count(commenter, name = "n_comments")
repeaters <- counts %>% filter(n_comments > 1) %>% pull(commenter)
rep_text <- named %>% filter(commenter %in% repeaters) %>% pull(text)
one_text <- named %>% filter(!commenter %in% repeaters) %>% pull(text)

comment_form <- bind_rows(
  row("emoji only", sum(is_emoji_only(texted$text)), n_all,
      "counted among comments carrying text, expressed over the full corpus"),
  row("contains a question mark", sum(str_detect(texted$text, fixed("?"))), n_all),
  row("two words or fewer",       sum(wc <= 2),  n_all),
  row("fifteen words or more",    sum(wc >= 15), n_all),
  row("repeat commenters, share of commenters", length(repeaters), nrow(counts)),
  row("repeat commenters, share of comments",
      sum(counts$n_comments[counts$n_comments > 1]), sum(counts$n_comments)),
  row("emoji only among repeat commenters",  sum(is_emoji_only(rep_text)), length(rep_text)),
  row("emoji only among one-off commenters", sum(is_emoji_only(one_text)), length(one_text))
)

comment_form
# 41.2% of the corpus is emoji and nothing else, 56.0% runs to two words or fewer,
# and only 4.6% contains a question mark. Elaboration is rare, 9.3% reach fifteen
# words. Engagement is also concentrated: a quarter of commenters (25.1%) produce
# 57.5% of the comments, and those returning commenters are the more minimal ones,
# 50.7% of their comments are emoji only against 29.7% for one-off commenters.
# The dominant response is affirmation without content, and it comes from a small
# returning audience.
write_csv(comment_form, here("output/tables/eng_comment_form.csv"))


# ---- 4. Comment topics under call-to-action types ----------------------------
# BERTopic labels come from the exploratory comment model. Joining them to the
# post-level primary CTA shows what kinds of comments were retrieved under posts
# that requested donations or volunteering. Topic -1 is BERTopic's heterogeneous
# residual cluster and remains visible rather than being reassigned by hand.
comment_topics <- read_csv(here("data/study1/features/comments_labeled.csv"),
                           show_col_types = FALSE) %>%
  filter(in_corpus == 1) %>%
  mutate(topic = as.integer(topic))
topic_info <- read_csv(here("data/study1/features/comment_topics_info.csv"),
                       show_col_types = FALSE) %>%
  mutate(topic = as.integer(topic)) %>%
  dplyr::select(topic, topic_name = name, top_words)
post_cta <- read_delim(here("data/study1/analysis_base.csv"), delim = ";",
                       show_col_types = FALSE) %>%
  filter(coded == 1) %>%
  transmute(Post_ID, CTA_Type = replace_na(m_Primary_CTA, "None"))

comments_with_cta <- comment_topics %>%
  inner_join(post_cta, by = "Post_ID") %>%
  left_join(topic_info, by = "topic") %>%
  mutate(topic_name = coalesce(topic_name, "unassigned"),
         top_words = coalesce(top_words, ""))

comment_topics_by_cta <- comments_with_cta %>%
  count(CTA_Type, topic, topic_name, top_words, name = "n_comments") %>%
  group_by(CTA_Type) %>%
  mutate(pct_within_cta = round(100 * n_comments / sum(n_comments), 1)) %>%
  ungroup() %>%
  arrange(CTA_Type, desc(n_comments), topic)

comment_cta_summary <- comments_with_cta %>%
  group_by(CTA_Type) %>%
  summarise(n_posts_with_comments = n_distinct(Post_ID),
            n_comments = n(),
            .groups = "drop") %>%
  arrange(desc(n_comments))

write_csv(comment_topics_by_cta, here("output/tables/eng_comment_topics_by_cta.csv"))
write_csv(comment_cta_summary, here("output/tables/eng_comment_cta_summary.csv"))

comment_cta_summary
comment_topics_by_cta %>% filter(CTA_Type %in% c("Donate", "Volunteer")) %>%
  group_by(CTA_Type) %>%
  slice_max(n_comments, n = 8, with_ties = FALSE) %>%
  ungroup()
