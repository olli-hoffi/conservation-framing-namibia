# =============================================================================
# 01_study1/06_message_construction.R
#
# WHERE THIS BELONGS
#   In the thesis: Chapter 3, Results: Message Construction and Craft
#   Produces: Table 4
#   Status: Exploratory. Study 1 carries no preregistration at all, so every
#   association here is descriptive and none of it tests a hypothesis that was
#   stated in advance.
#
# Study 1 (content analysis, PURELY EXPLORATORY, no preregistration): how the
# message itself is built, at the level of the POST.
#
# Seven descriptive views of message construction:
#   1. Readability (Flesch-Kincaid grade, Flesch ease, Gunning fog).
#   2. Voice / agency (pronoun rates, direct address, imperatives, questions).
#   3. Emotional register (NRC lexicon) + convergent checks against LLM frames.
#   4. Moral framing (Moral Foundations Dictionary, five foundations).
#   5. Emoji + hashtag strategy.
#   6. Frame co-occurrence bundles (phi coefficient + network).
#   7. Image craft (colour aesthetics, recycled covers) + a proxy validation.
#
# CAVEAT (dictionary measures are lexical proxies): the NRC emotion counts and the
# Moral Foundations Dictionary hits are word-list matches, NOT validated
# psychological instruments. They approximate the corpus's tone and are reported
# descriptively only; nothing here supports a preregistered or causal claim.
#
# Input : data/study1/analysis_base.csv               (one row per post; frame flags)
#         data/study1/features/caption_features.csv    (readability, pronouns, NRC, MFD)
#         data/study1/features/image_features.csv      (colour, near-duplicate covers)
#         data/study1/features/caption_hashtags_long.csv (hashtag class per tag)
# Output: output/tables/msg_voice_ci.csv, msg_nrc_ci.csv, msg_mfd_ci.csv,
#         msg_frame_cooccurrence.csv (these four + APA .docx),
#         msg_voice_shares.csv, msg_table6_dispersion.csv,
#         msg_convergent_checks.csv, msg_text_edge_validation.csv
# =============================================================================

# igraph builds the frame co-occurrence network; tidygraph + ggraph draw it inside
# the APA theme (ggrepel offsets the node labels); viridisLite gives palettes.
pacman::p_load(here, tidyverse, igraph, tidygraph, ggraph, ggrepel, viridisLite, flextable, officer)
list.files(here("src"), full.names = TRUE) %>% walk(source)

# The bootstrap CIs and the network layout both draw random numbers; fixing the
# seed makes every run reproduce the same intervals and the same node positions.
set.seed(42)

# analysis_base.csv is semicolon-delimited (captions contain commas); keep coded posts.
base <- read_delim(here("data/study1/analysis_base.csv"), delim = ";", show_col_types = FALSE) %>%
  filter(coded == 1)
caption_feat <- read_csv(here("data/study1/features/caption_features.csv"), show_col_types = FALSE)   # readability, pronouns, NRC, MFD per post
image_feat   <- read_csv(here("data/study1/features/image_features.csv"), show_col_types = FALSE)     # colour / brightness / near-dup per post

frames <- c("Empathy", "Threat", "Efficacy", "Collective_ID", "Normative", "Moral",
            "Economic", "Scientific", "Youth_Addressed", "Interactivity",
            "Youth_Style", "Protagonist", "Onscreen_Text", "Data_Visual")
# across(...) coerces every frame flag to a 0/1 integer; suppressWarnings hides the
# coercion warning on odd values.
base <- base %>% mutate(across(paste0("f_", frames), ~ suppressWarnings(as.integer(.x))))

# Join the codebook frames to the Python-derived caption + image features by Post_ID.
posts <- base %>% dplyr::select(Post_ID, Actor_Name, Actor_Type, Post_Format, eng_rate,
                         starts_with("f_")) %>%
  left_join(caption_feat, by = "Post_ID") %>%
  left_join(image_feat, by = "Post_ID")

nrow(base); sum(!is.na(posts$fk_grade)); sum(!is.na(posts$brightness))
# 917 coded posts; 896 carry caption features, 911 carry image features (a handful
# have no readable caption / cover image).

# Generic bootstrap-CI helper: resample values with replacement R times, apply stat
# (mean or median) each time, and return c(point estimate, 2.5%, 97.5%). Distribution-
# free, so it fits skewed lexical counts where a normal-theory CI would be wrong.
boot_ci <- function(values, stat = mean, R = 2000) {
  values <- values[!is.na(values)]; if (!length(values)) return(c(NA, NA, NA))
  boot <- replicate(R, stat(sample(values, replace = TRUE)))
  c(stat(values), quantile(boot, .025), quantile(boot, .975))
}


# ---- 1. Readability ----------------------------------------------------------
# FK grade / Gunning fog = approximate US school grade needed to read the caption;
# Flesch ease = 0-100 (higher = easier). Median + bootstrap CI on FK grade.
readability_ci <- boot_ci(posts$fk_grade, median)
tibble(fk_grade_med = readability_ci[1], fk_lo = readability_ci[2], fk_hi = readability_ci[3],
       flesch_ease_med = median(posts$flesch_ease, na.rm = TRUE),
       gunning_fog_med = median(posts$gunning_fog, na.rm = TRUE)) %>%
  mutate(across(everything(), ~ round(.x, 1)))
# FK grade median 10.6 [10.2, 11.0], Flesch ease median 48.5, Gunning fog median
# 12.8: captions sit at roughly a US grade-10/11 reading level (fairly demanding).
# [not reported, partial] all three agree that the captions are demanding. One
# reading level carries that point, and the Flesch-Kincaid grade of 10.6 is
# reported in Table 4. Flesch ease and Gunning fog are not.

# ---- Readability by post format [not reported] -------------------------------
# Splits the reading level by carousel, image and video.
readability_by_format <- posts %>% group_by(Post_Format) %>%
  summarise(n = n(), fk_median = round(median(fk_grade, na.rm = TRUE), 1),
            ease_median = round(median(flesch_ease, na.rm = TRUE), 0), .groups = "drop")
readability_by_format
# Carousels read hardest (FK 11.9, ease 43), then images (9.9, 51) and video (9.4, 55).
# -> the more text-forward the format, the higher the reading grade. One
#    corpus-wide reading level is reported, without a split by format.


# ---- 2. Voice / agency -------------------------------------------------------
# Pronoun rates per 100 words = the message's "voice": we (institutional), you
# (direct address), I (personal), they (third person). Mean + bootstrap 95% CI each.
voice <- tibble(marker = c("we (institutional)", "you (direct address)", "I (personal)", "they (third person)"),
                col    = c("pron_we", "pron_you", "pron_i", "pron_they")) %>%
  mutate(stat = map(col, ~ boot_ci(posts[[.x]], mean)),          # one bootstrap per pronoun column
         mean = map_dbl(stat, 1), lo = map_dbl(stat, 2), hi = map_dbl(stat, 3)) %>%   # unpack estimate + CI
  dplyr::select(marker, mean, lo, hi)

voice %>% mutate(across(where(is.numeric), ~ round(.x, 3)))
# Institutional "we" dominates the voice (1.85 per 100 words [1.69, 2.03]), roughly
# double direct-address "you" (0.90 [0.76, 1.06]); "they" 0.72 and personal "I" 0.21
# trail. The corpus speaks as an organisation more than to the reader.

# Share of posts that use direct address / an imperative / a question at all (>0 = present).
share_ci <- function(flag) {
  flag <- flag[!is.na(flag)]
  ci <- prop.test(sum(flag), length(flag), correct = FALSE)$conf.int  # Wilson score
  tibble(pct = 100 * mean(flag), lo = 100 * ci[1], hi = 100 * ci[2])
}
voice_shares <- bind_rows(
  share_ci(posts$pron_you > 0)      %>% mutate(marker = "direct address present"),
  share_ci(posts$n_imperatives > 0) %>% mutate(marker = "imperative present"),
  share_ci(posts$n_questions > 0)   %>% mutate(marker = "question present")
) %>%
  relocate(marker)

voice_shares
# Direct address 27.7% [24.9, 30.7], imperative 20.4% [17.9, 23.1], question
# 16.0% [13.8, 18.5] of the 917 coded posts: explicit calls to the reader are
# the exception. The imperative row feeds Table 4.

write_csv(voice_shares, here("output/tables/msg_voice_shares.csv"))
# Only 28% of posts use direct address, 20% an imperative, 16% a question: explicit
# calls to the reader are the exception, not the rule.

write_csv(voice, here("output/tables/msg_voice_ci.csv"))
save_apa_table(voice, "msg_voice_ci",
               title = "Table. Message Voice (Pronoun Rate per 100 Words, Mean + 95% CI)",
               note  = "Bootstrap 95% CIs (2000 resamples). Institutional we vs direct-address you.",
               digits = 3)


# ---- 3. NRC emotion profile + frame validation -------------------------------
# NRC lexicon: mean word-frequency per emotion (how often emotion-tagged words
# appear), with bootstrap CI, sorted, to describe the corpus's emotional register.
emotions <- c("anger", "fear", "joy", "trust", "anticipation", "sadness", "disgust", "surprise")
nrc_emotion <- tibble(emotion = emotions) %>%
  mutate(stat = map(emotion, ~ boot_ci(posts[[paste0("nrc_", .x)]], mean)),
         mean = map_dbl(stat, 1), lo = map_dbl(stat, 2), hi = map_dbl(stat, 3)) %>%
  dplyr::select(emotion, mean, lo, hi) %>% arrange(desc(mean))

nrc_emotion %>% mutate(across(where(is.numeric), ~ round(.x, 4)))
# The register is positive/approach-oriented: trust (.155) and anticipation (.118)
# lead, then joy (.079); the negative emotions fear (.046), sadness (.030), anger
# (.025) and disgust (.017) are far rarer. Conservation posts sound hopeful, not fearful.

write_csv(nrc_emotion, here("output/tables/msg_nrc_ci.csv"))
save_apa_table(nrc_emotion, "msg_nrc_ci",
               title = "Table. Emotional Register (NRC Lexicon, Mean Frequency + 95% CI)",
               note  = "Lexical proxy only. Bootstrap 95% CIs (2000 resamples).",
               digits = 4)

# Convergent validity: does a dictionary emotion track the matching LLM frame?
# point_biserial() = Pearson r between a continuous score and a 0/1 flag (a
# point-biserial correlation for a binary group); e.g. does nrc_fear rise on
# Threat-framed posts. Returns r + its 95% CI.
point_biserial <- function(score, flag) {
  ok <- !is.na(score) & !is.na(flag)
  ct <- suppressWarnings(cor.test(score[ok], flag[ok]))   # default Pearson = point-biserial for a binary flag
  # n and p travel with the coefficient so a reported r can be traced to the pairs
  # it rests on. Elements 1-3 keep their positions, so existing callers are unaffected.
  c(unname(ct$estimate), ct$conf.int[1], ct$conf.int[2], sum(ok), ct$p.value)
}

# ---- Convergent checks, NRC emotion against the frame codes [numbers not printed] ----
# Do the automatic emotion scores line up with the human frame codes?
convergent_checks <- tribble(
  ~pair,                    ~score,      ~flag,
  "nrc_fear ~ f_Threat",    "nrc_fear",  "f_Threat",
  "nrc_joy ~ f_Empathy",    "nrc_joy",   "f_Empathy",
  "nrc_trust ~ f_Efficacy", "nrc_trust", "f_Efficacy"
) %>%
  mutate(s  = map2(score, flag, ~ point_biserial(posts[[.x]], posts[[.y]])),
         r  = map_dbl(s, 1), lo = map_dbl(s, 2), hi = map_dbl(s, 3),
         n  = map_dbl(s, 4), p = map_dbl(s, 5)) %>%
  dplyr::select(pair, r, lo, hi, n, p)

convergent_checks %>% mutate(across(where(is.numeric), ~ round(.x, 3)))
# Written out because these coefficients are quoted in the Results. A reported
# number that lives only in a script comment cannot be checked against anything,
# and rounding a rounded value is how .1848 turns into .19 instead of .18.
write_csv(convergent_checks, here("output/tables/msg_convergent_checks.csv"))
# Fear tracks threat (r = .21 [.15, .27]) and trust tracks efficacy
# (r = .18 [.12, .25]). Joy does not track empathy (r = .01 [-.05, .08]).
# -> dictionary emotion mirrors the coded frames only partly, a limit on how far
#    automated affect can stand in for coding. Stated in words, without the
#    coefficients.


# ---- 4. Moral Foundations profile --------------------------------------------
# Moral Foundations Dictionary: mean word hits per 100 words for the five
# foundations (care, fairness, loyalty, authority, sanctity). Mean + CI, sorted.
mfd_profile <- tibble(foundation = c("care", "fairness", "loyalty", "authority", "sanctity")) %>%
  mutate(stat = map(foundation, ~ boot_ci(posts[[paste0("mfd_", .x)]], mean)),
         mean = map_dbl(stat, 1), lo = map_dbl(stat, 2), hi = map_dbl(stat, 3)) %>%
  dplyr::select(foundation, mean, lo, hi) %>% arrange(desc(mean))

mfd_profile %>% mutate(across(where(is.numeric), ~ round(.x, 3)))
# Care dominates the moral register (1.60 hits/100w [1.36, 1.86]), followed by
# sanctity (1.31) and loyalty (1.22); authority (.70) and fairness (.21) are minor.
# The moralisation of conservation runs mainly through care/harm, as theory expects.

write_csv(mfd_profile, here("output/tables/msg_mfd_ci.csv"))
save_apa_table(mfd_profile, "msg_mfd_ci",
               title = "Table. Moral Framing (Moral Foundations Dictionary, Hits/100w + 95% CI)",
               note  = "Lexical proxy only. Bootstrap 95% CIs (2000 resamples).",
               digits = 3)


# ---- 5. Emoji use in captions [not reported] ---------------------------------
# Share of captions carrying at least one emoji.
tibble(emoji_pct = 100 * mean(posts$n_emoji > 0, na.rm = TRUE)) %>%
  mutate(across(everything(), ~ round(.x, 0)))
# 47% carry at least one.
# -> decoration is common but not universal, so it separates no accounts. Hashtag
#    presence and count are reported in Table 4, where an interval can be formed.

# ---- Hashtag classes [not reported] ------------------------------------------
# Splits hashtags into community, branded, campaign and generic.
hashtag_classes <- read_csv(here("data/study1/features/caption_hashtags_long.csv"), show_col_types = FALSE) %>%
  count(tag_class) %>% mutate(pct = round(100 * n / sum(n)))
hashtag_classes
# Community and other 53%, branded 35%, campaign 4%, generic 8%.
# -> tags signal identity and belonging rather than campaign mobilisation.
#    Hashtag presence and count are reported, not what the tags say.


# ---- 6. Frame co-occurrence bundles ------------------------------------------
# phi coefficient = Pearson correlation between two binary variables; computed for
# every frame pair, it shows which frames tend to appear together.
frame_matrix <- base %>% dplyr::select(all_of(paste0("f_", frames))) %>% drop_na()   # complete-case 0/1 frame matrix
names(frame_matrix) <- frames
phi <- cor(frame_matrix)   # phi = Pearson r for binaries

# Long edge list: keep each unordered pair once (a < b avoids duplicates and self-
# pairs), sort by strength.
n_cooccurrence <- nrow(frame_matrix)   # complete-case posts behind EVERY pair

# Fisher z 95% CI for a correlation: z = atanh(r) is approximately normal with
# SE = 1/sqrt(n - 3), so the interval is built on the z scale and transformed back
# with tanh(). Reported because a phi read without its interval says nothing about
# how firmly the co-occurrence is estimated.
fisher_ci <- function(r, n, level = 0.95) {
  se <- 1 / sqrt(n - 3)
  z  <- qnorm(1 - (1 - level) / 2)
  list(low = tanh(atanh(r) - z * se), high = tanh(atanh(r) + z * se))
}

frame_cooccurrence <- as.data.frame(as.table(phi)) %>%
  filter(as.character(Var1) < as.character(Var2)) %>%
  rename(a = Var1, b = Var2, phi = Freq) %>%
  mutate(
    n         = n_cooccurrence,   # identical for all pairs: the matrix is complete-case
    ci_low    = fisher_ci(phi, n_cooccurrence)$low,
    ci_high   = fisher_ci(phi, n_cooccurrence)$high
  ) %>%
  arrange(desc(phi))

head(frame_cooccurrence, 8) %>% mutate(across(c(phi, ci_low, ci_high), ~ round(.x, 3)))
# Strongest positive bundles: Empathy-Threat (phi = .37), Efficacy-Threat (.31),
# Empathy-Protagonist (.30), Collective_ID-Efficacy (.24), Collective_ID-Moral (.22).
# Emotional (empathy/threat/protagonist) and mobilising (efficacy/collective/moral)
# frames each travel together, echoing the PCA structure in 01_study1/04_corpus_frame_prevalence.R.

write_csv(frame_cooccurrence, here("output/tables/msg_frame_cooccurrence.csv"))
save_apa_table(head(frame_cooccurrence, 12), "msg_frame_cooccurrence",
               title = "Table. Frame Co-occurrence (phi, Strongest Pairs)",
               note  = paste0("phi = Pearson correlation between two binary frame flags. Top pairs by ",
                              "strength. n = ", n_cooccurrence, " complete-case posts for every pair; ",
                              "95% CI via Fisher z transformation."),
               digits = 3)

# Network of |phi| >= .15: keep only edges above a weak-noise threshold for plotting.
strong_edges     <- frame_cooccurrence %>% filter(abs(phi) >= .15)
cooccur_network  <- graph_from_data_frame(strong_edges %>% transmute(from = a, to = b, weight = phi),
                                          directed = FALSE)


# ---- 7. Image craft [not reported] -------------------------------------------
# Median cover brightness, saturation, colourfulness and warm-tone share by format,
# plus a text-edge density proxy for on-image text. Non-image posts = NA.
image_craft <- posts %>% filter(!is.na(brightness)) %>% group_by(Post_Format) %>%
  summarise(n = n(), brightness = round(median(brightness), 2), saturation = round(median(saturation), 2),
            colourfulness = round(median(colourfulness), 0), warm = round(median(warm_share), 2),
            text_edge = round(median(text_edge_density), 3), .groups = "drop")
image_craft
# Covers are bright and warm-toned throughout. Video covers are darkest (.47), most
# saturated (.33) and carry the least on-image text (.052).
# -> a consistent visual register across formats, with video the darkest and least
#    text-laden. The coded visual devices are reported, not these pixel measures.

# ---- Text-edge validation [not reported] -------------------------------------
# Does the cheap edge-density proxy agree with the model's on-screen-text code?
text_edge_validation <- point_biserial(posts$text_edge_density, posts$f_Onscreen_Text)
text_edge_check <- tibble(pair = "text_edge_density ~ f_Onscreen_Text",
                          r  = text_edge_validation[1], lo = text_edge_validation[2],
                          hi = text_edge_validation[3], n  = text_edge_validation[4],
                          p  = text_edge_validation[5])
text_edge_check %>% mutate(across(where(is.numeric), ~ round(.x, 3)))
write_csv(text_edge_check, here("output/tables/msg_text_edge_validation.csv"))
# r = .18 [.12, .25], n = 873. Round from the full value (.1846), never from a value
# already rounded to three places, or .185 becomes .19.
# -> weak but positive, enough to treat the proxy as a rough stand-in. The model's
#    own code carries every reported visual result, so the proxy stays a check.

# Share of near-duplicate ("recycled") cover images per org (orgs with >= 15 posts).
recycled_covers <- image_feat %>% group_by(Actor_Name) %>%
  summarise(n = n(), recycled_pct = round(100 * mean(is_near_dup, na.rm = TRUE)), .groups = "drop") %>%
  filter(n >= 15) %>% arrange(desc(recycled_pct))
head(recycled_covers, 6)
# Recycled-cover rate is generally low; highest for Giraffe Conservation Foundation
# (15%) and N/a'an ku se Foundation (14%), then <= 5% for the rest. Most orgs use
# fresh cover images rather than reposting a template.


# =============================================================================
# (8) Dispersion for the Table 4 point estimates
# -----------------------------------------------------------------------------
# Every point estimate in Table 4 carries a measure of variability. APA 7 section
# 6.4 asks for one beside each estimate and section 6.44 for an interval wherever
# one can be formed, and neither carves out a descriptive table.
#
# Medians take a percentile bootstrap over posts, matching boot_ci() above.
# Proportions take a Wilson score interval, matching the frame prevalences of
# 01_study1/04_corpus_frame_prevalence.R. Each quantity therefore carries the same
# kind of interval the chapter uses for that kind of quantity elsewhere.
# =============================================================================

set.seed(42)

med_row <- function(label, v, digits = 2) {
  v <- v[!is.na(v)]
  ci <- boot_ci(v, median, R = 5000)
  tibble(quantity = label, kind = "median", n = length(v),
         estimate = round(ci[1], digits), lo = round(ci[2], digits), hi = round(ci[3], digits),
         iqr_low = round(quantile(v, .25), digits), iqr_high = round(quantile(v, .75), digits))
}

wilson_row <- function(label, k, n) {
  # Plain Wilson score interval, no continuity correction. Both this script and
  # 01_study1/04_corpus_frame_prevalence.R now use it, so the one label the chapter
  # applies ("Wilson score interval") describes one estimator. The uncorrected form is
  # what Wilson (1927) defines and what L. D. Brown et al. (2001), cited in the method
  # for exactly this choice, recommend; they reject the continuity-corrected variant.
  ci <- prop.test(k, n, correct = FALSE)$conf.int
  tibble(quantity = label, kind = "percent", n = n,
         estimate = round(100 * k / n, 1), lo = round(100 * ci[1], 1), hi = round(100 * ci[2], 1),
         iqr_low = NA_real_, iqr_high = NA_real_)
}

img_by_format <- function(label, col, fmt) {
  s <- posts %>% filter(Post_Format == fmt, !is.na(.data[[col]]))
  med_row(label, s[[col]], digits = 2)
}

table6_dispersion <- bind_rows(
  med_row("Flesch-Kincaid grade level, median", posts$fk_grade, 1),
  med_row("Caption length in words, median", posts$n_words_clean, 0),
  med_row("Hashtags per post among users, median",
          posts$n_hashtags[posts$n_hashtags > 0], 0),
  med_row("Warm-hue share, median", posts$warm_share, 2),
  img_by_format("Brightness, median (video)", "brightness", "Video"),
  img_by_format("Brightness, median (photograph)", "brightness", "Image"),
  img_by_format("Saturation, median (video)", "saturation", "Video"),
  img_by_format("Saturation, median (photograph)", "saturation", "Image"),
  wilson_row("Carries at least one number, %",
             sum(posts$n_numbers > 0, na.rm = TRUE), sum(!is.na(posts$n_numbers))),
  wilson_row("Carries at least one hashtag, %",
             sum(posts$n_hashtags > 0, na.rm = TRUE), sum(!is.na(posts$n_hashtags))),
  wilson_row("English-language captions, %",
             sum(base$m_Language == "English", na.rm = TRUE),
             sum(!is.na(base$m_Language) & base$m_Language != "")),
  wilson_row("On-image data visualization, %",
             sum(base$f_Data_Visual == 1, na.rm = TRUE), sum(!is.na(base$f_Data_Visual)))
)

table6_dispersion
write_csv(table6_dispersion, here("output/tables/msg_table6_dispersion.csv"))
