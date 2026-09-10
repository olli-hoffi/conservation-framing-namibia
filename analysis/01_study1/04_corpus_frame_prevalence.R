# =============================================================================
# 01_study1/04_corpus_frame_prevalence.R
#
# WHERE THIS BELONGS
#   In the thesis: Chapter 3, Results: Corpus Composition and A Priori Frame Prevalence
#   Produces: Table 3
#   Status: Exploratory. Study 1 carries no preregistration at all, so every
#   association here is descriptive and none of it tests a hypothesis that was
#   stated in advance.
#
# Study 1 (content analysis, PURELY EXPLORATORY, no preregistration): does the
# a-priori frame / CTA / story taxonomy fit the corpus?
#
# Asks three descriptive questions:
#   1. Frame prevalence: how common is each coded frame, with a 95% CI, and are any
#      so rare (floor) or near-universal (ceiling) that they cannot discriminate?
#   2. Corpus composition: the six single-choice variables that describe what the
#      corpus is made of.
#   3. Actor type: empathy framing and call-to-action rates for the two categories
#      that carry more than one organization.
#
# Input : data/study1/analysis_base.csv   (master per-post table, semicolon-delimited)
# Output: output/tables/codebook_frame_prevalence.csv,
#         codebook_corpus_composition.csv,
#         codebook_actortype_frames.csv (+ APA .docx)
# =============================================================================

pacman::p_load(here, tidyverse, flextable, officer)
list.files(here("src"), full.names = TRUE) %>% walk(source)

# delim = ";" because captions contain commas, so the export is semicolon-separated.
df    <- read_delim(here("data/study1/analysis_base.csv"), delim = ";", show_col_types = FALSE)
coded <- df %>% filter(coded == 1)   # keep only posts that were actually coded

nrow(coded)
# 917 coded posts, the corpus all Study 1 descriptives are computed on.

# The 14 message-frame variables (columns carry an "f_" prefix). This order fixes
# their order everywhere downstream.
frames <- c("Empathy", "Threat", "Efficacy", "Collective_ID", "Normative", "Moral",
            "Economic", "Scientific", "Youth_Addressed", "Interactivity",
            "Youth_Style", "Protagonist", "Onscreen_Text", "Data_Visual")
fcols  <- paste0("f_", frames)

# across(all_of(fcols), ...) coerces every frame column to integer 0/1;
# suppressWarnings hides the coercion warning on odd values.
coded <- coded %>% mutate(across(all_of(fcols), ~ suppressWarnings(as.integer(.x))))


# ---- 1. Frame prevalence + Wilson 95% CI ------------------------------------
# map_dfr = run the function per frame and row-bind the one-row results.
frame_prevalence <- map_dfr(frames, function(v) {
  x  <- coded[[paste0("f_", v)]]
  x  <- x[!is.na(x)]                     # drop NAs from this frame's 0/1 column
  # prop.test() returns the proportion present plus a score-based ("Wilson") 95% CI,
  # which behaves better than a normal approximation at extreme rates. correct = FALSE
  # gives the plain Wilson interval. The default would apply a continuity correction,
  # which is a different estimator under the same name and which L. D. Brown et al.
  # (2001), cited in the method for this choice, argue against.
  pt <- prop.test(sum(x), length(x), correct = FALSE)
  tibble(frame = v, n = length(x), k = sum(x), prev = sum(x) / length(x),
         ci_lo = pt$conf.int[1], ci_hi = pt$conf.int[2])
}) %>%
  arrange(desc(prev)) %>%
  # Flag frames too rare (< 3%) or too common (> 80%) to discriminate: at those base
  # rates a variable carries almost no comparative information (floor / ceiling).
  mutate(flag = case_when(prev < .03 ~ "FLOOR (<3%)",
                          prev > .80 ~ "CEILING (>80%)",
                          TRUE       ~ ""))

frame_prevalence %>% mutate(across(where(is.numeric), ~ round(.x, 3)))
# Most common: Protagonist 64.8%, Efficacy 49.6%, Collective_ID 29.0%, Scientific
# 28.6%, Onscreen_Text 22.6%, Threat 19.4%, Empathy 17.7%. Four FLOOR frames (< 3%):
# Interactivity 1.7%, Youth_Style 1.6%, Normative 0.7%, Data_Visual 0.5%. No ceiling.
# -> the corpus leans on protagonist/efficacy framing; the four floor frames are near-
#    absent in the repertoire (a substantive finding, and too rare to discriminate).

write_csv(frame_prevalence, here("output/tables/codebook_frame_prevalence.csv"))

save_apa_table(frame_prevalence, "codebook_frame_prevalence",
               title = "Table. Frame Prevalence With Wilson 95% Confidence Intervals",
               note  = "prev = proportion of coded posts using the frame. Floor < 3%, ceiling > 80%.",
               digits = 3)


# ---- 2. Corpus composition --------------------------------------------------
# The six single-choice variables that describe what the corpus is made of. The
# thesis reports these as one table instead of three paragraphs of running counts,
# so they need one source.
composition_cols <- c("Post format"          = "Post_Format",
                      "Actor type"           = "Actor_Type",
                      "Primary topic"        = "m_Primary_Topic",
                      "Emotional valence"    = "m_Emotional_Valence",
                      "Primary call to action" = "m_Primary_CTA",
                      "Story structure"      = "m_Story_Structure")

composition <- imap_dfr(composition_cols, function(col, label) {
  coded %>%
    transmute(category = as.character(.data[[col]])) %>%
    # "" and "?" both mean the coder assigned no category; the thesis reports them as one row
    mutate(category = if_else(is.na(category) | category %in% c("", "?"), "unassigned", category)) %>%
    count(category, sort = TRUE) %>%
    mutate(characteristic = label, pct = round(n / nrow(coded) * 100, 1))
}) %>%
  dplyr::select(characteristic, category, n, pct)

composition %>% write_csv(here("output", "tables", "codebook_corpus_composition.csv"))
# Each characteristic sums to N = 917. These categories are mutually exclusive,
# unlike the frames above, which may co-occur.
stopifnot(all(composition %>% count(characteristic, wt = n) %>% pull(n) == nrow(coded)))


# ---- 3. Empathy framing and calls to action by actor type -------------------
# Chapter 3 reports two actor types descriptively, the only two the frame carries
# enough accounts for. The other four rest on a single organization each, so a
# figure for them would describe that organization and not its category. This
# block is the source for those numbers.
#
# Rates are post-level over the 917 coded posts, not organization-level averages.
# m_Primary_CTA is a single-choice variable whose "None" level means the post asked
# nothing of the reader, so the ask rate is the complement of that level.

# The numeric Actor_Type in analysis_base carries its label in the org feature
# matrix, the same mapping 05_org_styles_genres.R uses for org_actortype_profile.
actor_labels <- read_csv(here("data/study1/features/org_feature_matrix.csv"),
                         show_col_types = FALSE) %>%
  distinct(Actor_Type, Actor_Type_label)

actortype_frames <- coded %>%
  mutate(Actor_Type = as.integer(Actor_Type)) %>%
  left_join(actor_labels, by = "Actor_Type") %>%
  group_by(Actor_Type, Actor_Type_label) %>%
  summarise(n_orgs      = n_distinct(Actor_Name),
            n_posts     = n(),
            empathy_pct = round(100 * mean(f_Empathy == 1, na.rm = TRUE), 1),
            ask_pct     = round(100 * mean(m_Primary_CTA != "None", na.rm = TRUE), 1),
            .groups = "drop") %>%
  # single-organization categories are reported as such rather than as a rate
  mutate(reportable = n_orgs > 1) %>%
  arrange(desc(n_posts))

actortype_frames

write_csv(actortype_frames, here("output/tables/codebook_actortype_frames.csv"))

save_apa_table(actortype_frames %>% dplyr::select(-Actor_Type),
               "codebook_actortype_frames",
               title = "Table. Empathy Framing and Call-to-Action Rates by Actor Type",
               note  = paste("Post-level rates over the 917 coded posts. empathy_pct = share of",
                             "posts carrying the empathy frame. ask_pct = share of posts with a",
                             "call to action other than None. reportable = FALSE marks the four",
                             "categories held by a single organization, where a rate would",
                             "describe that organization rather than the category."),
               digits = 1)

# Guard the two figures Chapter 3 states in prose. If a recode or a reclassified
# organization ever moves them, this stops the run instead of letting the text
# drift away from the data.
ngo <- actortype_frames %>% filter(Actor_Type_label == "NGO/Foundation/Trust")
umb <- actortype_frames %>% filter(Actor_Type_label == "Umbrella/Network")
stopifnot(
  ngo$n_orgs == 12, ngo$n_posts == 600, ngo$empathy_pct == 24.7, ngo$ask_pct == 32.3,
  umb$n_orgs ==  3, umb$n_posts ==  65, umb$empathy_pct ==  0.0, umb$ask_pct == 44.6,
  sum(!actortype_frames$reportable) == 4
)
# NGO/Foundation/Trust: 12 orgs, 600 posts, 24.7% empathy, 32.3% ask.
# Umbrella/Network: 3 orgs, 65 posts, 0% empathy, 44.6% ask.
# The remaining four categories hold one organization each.
