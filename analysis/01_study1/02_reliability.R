# =============================================================================
# 01_study1/02_reliability.R
#
# WHERE THIS BELONGS
#   In the thesis: Chapter 3, Method: Reliability Assessment
#   Produces: Table 1
#   Status: Descriptive. It reports what the data contain rather than testing a
#   claim, so there is no hypothesis and no p value to correct.
#
# Study 1 (content analysis, PURELY EXPLORATORY, no preregistration). How well do
# the automated multimodal LLM codes match a human standard? The question has two
# answers, and Table 1 prints both side by side, which is why one script produces
# both.
#
# PART A, the ANCHORED standard. A human reviewer worked FROM the LLM-prefilled
# codes, correcting what looked wrong. Agreeing with a code already on screen is
# cheap, so these figures are an UPPER BOUND on accuracy, not independent
# inter-coder reliability. They measure a correction rate.
#
# PART B, the BLIND standard. A separate 184-post sample was coded from captions
# alone, with no LLM code visible. Human and model are two genuinely independent
# raters here, so Cohen's kappa means what it normally means.
#
# READ THE TWO TOGETHER. Part A is the ceiling, Part B is what survives
# independence, and the distance between them is the size of the anchoring effect.
# Part B computes that distance explicitly and writes it out.
#
# WHY BOTH kappa AND % agreement: Cohen's kappa can collapse toward 0 even at ~99%
# agreement when a variable is almost always 0 (or almost always 1), the "kappa
# paradox" or base-rate degeneracy. Two visual frames (Interactivity, Data_Visual)
# show near-total exact agreement yet kappa near 0. For those, % agreement is the
# honest number and the floor effect is itself a substantive finding.
#
# Input : data/study1/Study1_LLM_multimodal_920.csv       (automated LLM codes, banner line 1)
#         data/study1/Study1_LLM_multimodal_reviewed.csv  (human-verified subsample)
#         data/study1/Study1_BlindCoded_184.csv           (blind human codes, 184 posts)
# Output: output/tables/reliability_kappa.csv (+ APA .docx)
#         output/tables/reliability_kappa_blind.csv (+ APA .docx)
#         output/tables/reliability_kappa_blind_vs_anchored.csv
#         output/tables/reliability_kappa_blind_by_format.csv
# =============================================================================

# irr supplies kappa2() (Cohen's kappa for two raters); psych supplies
# cohen.kappa(), used here only for the asymptotic 95% CI around that kappa.
pacman::p_load(here, tidyverse, irr, psych, flextable, officer)
list.files(here("src"), full.names = TRUE) %>% walk(source)

# delim = ";" because captions contain commas, so the export is semicolon-separated.
# Do NOT set comment = "#": captions carry hashtag lines that a readr comment filter
# would treat as comments and corrupt the quoted multiline records. The 920 LLM file
# carries a single leading "#" banner line, so it is skipped explicitly with skip = 1.
read_semi <- function(p, skip = 0) read_delim(p, delim = ";", show_col_types = FALSE, skip = skip)

llm   <- read_semi(here("data/study1/Study1_LLM_multimodal_920.csv"), skip = 1)
rev   <- read_semi(here("data/study1/Study1_LLM_multimodal_reviewed.csv")) %>%
  filter(reviewed == 1)   # keep only the human-reviewed rows (the validation subsample)
blind <- read_semi(here("data/study1/Study1_BlindCoded_184.csv"))

nrow(llm)
# 920 automated LLM-coded posts, the corpus both human samples were drawn from.
nrow(rev)
# 101 human-reviewed posts, the anchored validation subsample of Part A.
nrow(blind)
# 184 blind-coded posts, the independent validation sample of Part B.

# Every human-coded post must exist in the LLM corpus, otherwise the join silently
# shrinks and the reported N would be a different sample than the one described.
# The blind file had this guard from the start. The anchored file did not, and an
# unescaped newline inside two captions had split those rows in the shipped CSV,
# so two posts lost their codes to a fragment and the sample quietly fell to 97.
stopifnot(all(blind$Post_ID %in% llm$Post_ID))
stopifnot(all(rev$Post_ID %in% llm$Post_ID))

# 22 of the 184 blind posts also appear in the 101-post anchored review. The blind
# pass came FIRST, so those posts were coded from scratch before the reviewer ever
# saw model output, and recall from the anchored pass cannot have influenced them.
# The sensitivity kappa in Part B therefore checks a concern that does not arise,
# and it is retained only as a robustness report.
overlap_ids <- intersect(blind$Post_ID, rev$Post_ID)
length(overlap_ids)
# 22 posts appear in both human samples.

bin <- c("Empathy", "Threat", "Efficacy", "Collective_ID", "Normative", "Moral",
         "Economic", "Scientific", "Youth_Addressed", "Interactivity",
         "Youth_Style", "Protagonist", "Onscreen_Text", "Data_Visual")
mul <- c("Emotional_Valence", "Story_Structure", "Primary_CTA", "Primary_Topic", "Language")

# Landis & Koch verbal bands for a kappa value (the usual descriptive labels).
band <- function(k) case_when(
  is.na(k) ~ "undefined", k < .20 ~ "slight/none", k < .40 ~ "fair",
  k < .60 ~ "moderate", k < .80 ~ "substantial", TRUE ~ "almost perfect")

# For every variable, line up the human (MANUAL_*) vs LLM code on shared Post_IDs and
# compute % agreement + Cohen's kappa. map_dfr stacks one result row per variable.
# 95% CI for a kappa, from psych::cohen.kappa()'s asymptotic (Fleiss) standard
# error. Reported because a reliability coefficient without an interval hides how
# firmly it is estimated on 101 posts. Returns NA where the confusion matrix is
# degenerate (a near-constant column), i.e. where the SE is undefined rather than
# merely large - the same variables the kappa-paradox flag below catches.
kappa_ci <- function(d) {
  out <- try(suppressWarnings(psych::cohen.kappa(as.matrix(d))), silent = TRUE)
  if (inherits(out, "try-error") || is.null(out$confid)) return(c(NA_real_, NA_real_))
  ci <- out$confid["unweighted kappa", c("lower", "upper")]
  if (any(!is.finite(ci))) return(c(NA_real_, NA_real_))
  unname(ci)
}

# =============================================================================
# PART A, the anchored standard: LLM codes against the prefilled human review
# =============================================================================

reliability_kappa <- map_dfr(c(bin, mul), function(v) {
  col <- paste0("MANUAL_", v)
  j <- rev %>% dplyr::select(Post_ID, hv = all_of(col)) %>%                          # hv = human-verified value
    inner_join(llm %>% dplyr::select(Post_ID, lv = all_of(col)), by = "Post_ID") %>% # lv = LLM value
    mutate(hv = as.character(hv), lv = as.character(lv)) %>%
    filter(!is.na(hv), !is.na(lv), hv != "", lv != "")                        # keep rows both coders labelled
  if (nrow(j) < 5) return(tibble(variable = v, n = nrow(j)))                  # too few overlapping posts -> just record N
  k  <- suppressWarnings(irr::kappa2(j %>% dplyr::select(lv, hv))$value)             # Cohen's kappa for the 2 raters
  ci <- kappa_ci(j %>% dplyr::select(lv, hv))                                        # asymptotic 95% CI around it
  tibble(variable = v, type = if (v %in% bin) "binary" else "multiclass",
         n = nrow(j), agree = mean(j$lv == j$hv), kappa = k,                  # agree = raw % agreement
         kappa_ci_low = ci[1], kappa_ci_high = ci[2],
         changed = sum(j$lv != j$hv), band = band(k),                         # changed = cells the human corrected
         base_rate = if (v %in% bin) mean(j$hv == "1") else NA_real_)         # base_rate = share coded 1 (for the paradox check)
}) %>% arrange(type, desc(kappa))

# Flag base-rate-degenerate variables (the kappa paradox): raters agree >= 95% yet
# kappa is missing or < .40 because the variable is near-constant. Report % agreement
# for those and treat the floor effect as a finding, not a reliability failure.
reliability_kappa <- reliability_kappa %>% mutate(
  degenerate = type == "binary" & agree >= .95 & (is.na(kappa) | kappa < .40),
  note = if_else(degenerate, "kappa paradox (floor/ceiling base rate)", ""))

reliability_kappa %>% mutate(across(c(agree, kappa, base_rate), ~ round(.x, 3)))
# All 19 variables estimable. Binary kappas are strong (Normative & Youth_Addressed
# 1.00, Collective_ID .98, Efficacy .98, Scientific .97, Moral .97, Economic .88,
# Empathy .84, Threat .78; Protagonist .68 and Onscreen_Text .64 the hardest). The two
# degenerate visual frames Data_Visual (.99 agree, kappa .00) and Interactivity (.98
# agree, kappa -.01) are near-constant columns - the kappa paradox. Multiclass kappas
# are also high (Primary_CTA .98, Story_Structure .94, Primary_Topic .81,
# Emotional_Valence .79, Language .66).

write_csv(reliability_kappa, here("output/tables/reliability_kappa.csv"))
save_apa_table(reliability_kappa, "reliability_kappa",
               title = "Table. Human-Verified vs Multimodal LLM Coding Agreement (Cohen's Kappa)",
               note  = paste0("N = ", nrow(rev), " reviewed posts. Human verification worked from LLM ",
                              "prefill (upper bound on accuracy). agree = raw proportion agreement; ",
                              "degenerate = kappa paradox (near-constant base rate), report agree instead."),
               digits = 3)


# ---- Summary over the estimable (non-degenerate) variables ------------------

# nondeg = variables with an interpretable (non-paradox) kappa; the degenerate visual
# frames are excluded so they do not drag the mean toward 0.
nondeg <- reliability_kappa %>% filter(!degenerate, !is.na(kappa))

cells_changed  <- sum(reliability_kappa$changed, na.rm = TRUE)
cells_total    <- sum(reliability_kappa$n, na.rm = TRUE)
pct_changed    <- 100 * cells_changed / cells_total
c(changed = cells_changed, total = cells_total, pct = round(pct_changed, 1))
# 79 of 1868 coded cells adjusted by the reviewer = 4.2% correction rate: the LLM
# codes needed only light human touch-up.

mean_kappa_binary     <- mean(nondeg$kappa[nondeg$type == "binary"])
mean_kappa_multiclass <- mean(nondeg$kappa[nondeg$type == "multiclass"])
round(c(binary = mean_kappa_binary, multiclass = mean_kappa_multiclass), 3)
# Mean kappa (excl. degenerate): binary .864, multiclass .835 - both "almost perfect"
# on the Landis & Koch bands.

n_at_threshold <- sum(nondeg$kappa >= .70)
c(at_or_above_70 = n_at_threshold, estimable = nrow(nondeg))
# 13 of 17 estimable kappas clear the conventional kappa >= .70 acceptable-reliability
# bar (the 4 below: Protagonist .68, Language .66, Youth_Style .65, Onscreen_Text .64).

reliability_kappa$variable[reliability_kappa$degenerate]
# "Data_Visual" "Interactivity" - the two base-rate-degenerate visual frames: report
# their ~99% agreement, and read the near-absent base rate as a substantive finding.



# =============================================================================
# PART B, the blind standard: LLM codes against independent human coding
# =============================================================================

# One kappa per variable on the posts BOTH raters labelled. ids = restrict to a post
# subset, used below for the overlap-excluded sensitivity run.
kappa_table <- function(ids = NULL) {
  b <- if (is.null(ids)) blind else blind %>% filter(Post_ID %in% ids)
  map_dfr(c(bin, mul), function(v) {
    col <- paste0("MANUAL_", v)
    j <- b %>% dplyr::select(Post_ID, bv = all_of(col)) %>%                            # bv = blind human value
      inner_join(llm %>% dplyr::select(Post_ID, lv = all_of(col)), by = "Post_ID") %>% # lv = LLM value
      mutate(bv = as.character(bv), lv = as.character(lv)) %>%
      filter(!is.na(bv), !is.na(lv), bv != "", lv != "")                        # pairwise-complete only
    if (nrow(j) < 5) return(tibble(variable = v, n = nrow(j)))
    k  <- suppressWarnings(irr::kappa2(j %>% dplyr::select(lv, bv))$value)
    ci <- kappa_ci(j %>% dplyr::select(lv, bv))
    tibble(variable = v, type = if (v %in% bin) "binary" else "multiclass",
           n = nrow(j), agree = mean(j$lv == j$bv), kappa = k,
           kappa_ci_low = ci[1], kappa_ci_high = ci[2],
           disagree = sum(j$lv != j$bv), band = band(k),
           base_rate = if (v %in% bin) mean(j$bv == "1") else NA_real_)         # base rate per the HUMAN
  }) %>% arrange(type, desc(kappa))
}

reliability_blind <- kappa_table()

# Flag base-rate-degenerate variables (the kappa paradox): raters agree >= 95% yet
# kappa is missing or < .40 because the variable is near-constant. Report % agreement
# for those and treat the floor effect as a finding, not a reliability failure.
reliability_blind <- reliability_blind %>% mutate(
  degenerate = type == "binary" & agree >= .95 & (is.na(kappa) | kappa < .40),
  note = if_else(degenerate, "kappa paradox (floor/ceiling base rate)", ""))

reliability_blind %>% mutate(across(c(agree, kappa, base_rate), ~ round(.x, 3)))

write_csv(reliability_blind, here("output/tables/reliability_kappa_blind.csv"))
save_apa_table(reliability_blind, "reliability_kappa_blind",
               title = "Table. Blind Human vs Multimodal LLM Coding Agreement (Cohen's Kappa)",
               note  = paste0("N = ", nrow(blind), " posts coded blind (no LLM code visible). ",
                              "agree = raw proportion agreement; degenerate = kappa paradox ",
                              "(near-constant base rate), report agree instead. Pairwise-complete: ",
                              "cells left blank by the human coder are excluded per variable."),
               digits = 3)


# ---- Summary over the estimable (non-degenerate) variables ------------------

nondeg <- reliability_blind %>% filter(!degenerate, !is.na(kappa))

cells_disagree <- sum(reliability_blind$disagree, na.rm = TRUE)
cells_total    <- sum(reliability_blind$n, na.rm = TRUE)
c(disagree = cells_disagree, total = cells_total,
  pct = round(100 * cells_disagree / cells_total, 1))

round(c(binary     = mean(nondeg$kappa[nondeg$type == "binary"]),
        multiclass = mean(nondeg$kappa[nondeg$type == "multiclass"])), 3)

c(at_or_above_70 = sum(nondeg$kappa >= .70), estimable = nrow(nondeg))

reliability_blind$variable[reliability_blind$degenerate]


# ---- Anchored (Part A) vs blind (Part B): how big was the anchoring effect? ---------

# Same variables, same LLM codes, two different human passes. The drop from anchored
# to blind kappa is the part of Part A's agreement that the prefill was buying.
anchored_path <- here("output/tables/reliability_kappa.csv")
if (file.exists(anchored_path)) {
  anchored <- read_csv(anchored_path, show_col_types = FALSE) %>%
    dplyr::select(variable, type, n_anchored = n, agree_anchored = agree, kappa_anchored = kappa)

  blind_vs_anchored <- reliability_blind %>%
    dplyr::select(variable, type, n_blind = n, agree_blind = agree, kappa_blind = kappa) %>%
    inner_join(anchored, by = c("variable", "type")) %>%
    mutate(kappa_drop = kappa_anchored - kappa_blind,
           agree_drop = agree_anchored - agree_blind) %>%
    arrange(desc(kappa_drop))

  blind_vs_anchored %>% mutate(across(where(is.numeric), ~ round(.x, 3)))

  write_csv(blind_vs_anchored, here("output/tables/reliability_kappa_blind_vs_anchored.csv"))

  round(c(mean_kappa_anchored = mean(blind_vs_anchored$kappa_anchored, na.rm = TRUE),
          mean_kappa_blind    = mean(blind_vs_anchored$kappa_blind,    na.rm = TRUE),
          mean_drop           = mean(blind_vs_anchored$kappa_drop,     na.rm = TRUE)), 3)
} else {
  message("output/tables/reliability_kappa.csv not found - run Part A above first ",
          "to get the anchored-vs-blind comparison.")
}


# ---- Stratification by post format: did the two raters see the same stimulus? -

# The model received the COVER IMAGE alone. The blind interface embedded the live post,
# so it played video and advanced carousel frames. On carousel and video posts the two
# raters were therefore reading different material, and part of the disagreement has
# nothing to do with the codebook. Single-image posts are the matched-stimulus subset:
# there both raters saw exactly the same thing, so the kappa difference between the two
# strata separates a DISPLAY effect from a coding effect.
blind %>% count(Post_Format)

single_ids <- blind %>% filter(Post_Format == "Image") %>% pull(Post_ID)
c(n_single_image = length(single_ids), n_carousel_video = nrow(blind) - length(single_ids))

# Same kappa_table() as above, restricted to the matched-stimulus subset. The
# inner_join keeps only variables estimable under BOTH strata, so the two means below
# describe the same variables instead of two different sets.
blind_single <- kappa_table(ids = single_ids) %>%
  filter(!is.na(kappa)) %>%
  dplyr::select(variable, type, n_single = n, kappa_single = kappa,
         kappa_single_lo = kappa_ci_low, kappa_single_hi = kappa_ci_high)

# The matched-stimulus kappas carry their own intervals, kept here because the
# Results quote one of them (Protagonist) in prose and a bare coefficient without
# an interval is the gap APA 7 section 6.44 closes.
reliability_blind_by_format <- reliability_blind %>%
  filter(!is.na(kappa)) %>%
  dplyr::select(variable, type, n_all = n, kappa_all = kappa,
         kappa_all_lo = kappa_ci_low, kappa_all_hi = kappa_ci_high) %>%
  inner_join(blind_single, by = c("variable", "type")) %>%
  mutate(kappa_gain = kappa_single - kappa_all) %>%
  arrange(desc(kappa_gain))

reliability_blind_by_format %>% mutate(across(where(is.numeric), ~ round(.x, 3)))

# The visual variables gain most, which is what a display effect predicts: they are the
# ones the model could not see on a video or a later carousel frame. A variable that
# does NOT move on matched stimuli locates its failure in the category definition.
round(c(n_variables       = nrow(reliability_blind_by_format),
        mean_kappa_all    = mean(reliability_blind_by_format$kappa_all),
        mean_kappa_single = mean(reliability_blind_by_format$kappa_single),
        mean_gain         = mean(reliability_blind_by_format$kappa_gain)), 3)

write_csv(reliability_blind_by_format,
          here("output/tables/reliability_kappa_blind_by_format.csv"))


# ---- Sensitivity: prior exposure in the anchored pass [not reported] ---------
# Checks whether the coder's memory of the earlier anchored pass lifts blind kappa,
# by dropping the posts that appear in both. If blind kappas hold up on the posts
# the coder had never reviewed, recall is not driving the result.
sens <- kappa_table(ids = setdiff(blind$Post_ID, overlap_ids)) %>%
  filter(!is.na(kappa)) %>%
  dplyr::select(variable, type, n_clean = n, kappa_clean = kappa)

sens_cmp <- reliability_blind %>%
  filter(!is.na(kappa)) %>%
  dplyr::select(variable, type, n_all = n, kappa_all = kappa) %>%
  inner_join(sens, by = c("variable", "type")) %>%
  mutate(delta = kappa_clean - kappa_all)

sens_cmp %>% mutate(across(where(is.numeric), ~ round(.x, 3)))
round(c(mean_kappa_all = mean(sens_cmp$kappa_all), mean_kappa_clean = mean(sens_cmp$kappa_clean),
        mean_delta = mean(sens_cmp$delta)), 3)
# Mean blind kappa .51 across the full blind set and .46 without the overlap posts,
# a drop of .05.
# -> prior exposure is the obvious threat to a blind standard, so it is worth
#    quantifying. Robustness by post format is reported instead, kappa .53 across
#    all 184 blind posts against .63 on the 62 single-image posts.


