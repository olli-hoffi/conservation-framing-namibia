# =============================================================================
# 01_study1/03_reliability_stability.R
#
# WHERE THIS BELONGS
#   In the thesis: Chapter 3, Method: Reliability Assessment
#   Produces: Table B2
#   Status: Descriptive. It reports what the data contain rather than testing a
#   claim, so there is no hypothesis and no p value to correct.
#
# Study 1 - EXPLORATORY, descriptive.
#
# PURPOSE
# Table B2's "Image effect (pts)" column and the sentence in 3.2 that reports it
# ("For 11 of the 15 variables derivable from text, the two comparisons differed
# by no more than 1.3 percentage points, whereas Efficacy fell 13.9 points and
# Story structure 12.9 points once the image was withheld") are computed here from
# the caption-only and multimodal coding passes.
#
# WHAT THE QUANTITY IS
#   pairwise            mean pairwise exact agreement across the three stability
#                       passes of the multimodal instrument over the same 184
#                       posts. This is the run-to-run baseline: how much the same
#                       instrument disagrees with itself on identical input.
#   textonly_vs_run1    exact agreement between the earlier CAPTION-ONLY pass and
#                       the coding of record, the full-corpus multimodal pass from
#                       which every code in this thesis is taken.
#   image effect        pairwise - textonly_vs_run1, in percentage points.
#
# Positive values mean withholding the display image moves a code further than
# the instrument's own run-to-run noise does. The quantity is undefined for the
# four visual variables, which the caption-only pass could not code, and those
# rows carry an empty textonly_vs_run1.
#
# WHAT THIS IS NOT
# Not a test. No hypothesis, no interval, no p value. A descriptive difference of
# two agreement rates on the same 184 posts, reported as such in Table B2.
#
# WHERE THE INPUT COMES FROM
# data/study1/stability_results.csv has no producer in this package, and that is
# deliberate. It records three repeat passes plus one caption-only pass of the
# multimodal LLM instrument over the same 184 posts. Rerunning them would need the
# language-model CLI and would not return the same codes, since the instrument is
# not deterministic across runs, which is the very quantity measured here. The
# committed file is therefore the canonical record, in the same way as the outputs
# of code_all_posts_multimodal.py and b1_llm_taxonomy_pass.py.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(here)
})

stab <- read_csv(here("data/study1/stability_results.csv"), show_col_types = FALSE)

image_effect <- stab %>%
  filter(!is.na(textonly_vs_run1)) %>%
  transmute(
    variable,
    kind,
    n,
    pairwise_pct         = round(100 * pairwise, 1),
    textonly_vs_run1_pct = round(100 * textonly_vs_run1, 1),
    image_effect_pts     = round(100 * (pairwise - textonly_vs_run1), 1)
  ) %>%
  arrange(desc(image_effect_pts))

write_csv(image_effect, here("output/tables/study1_stability_image_effect.csv"))

# The two claims the prose makes, recomputed rather than asserted.
within_band <- sum(abs(image_effect$image_effect_pts) <= 1.3)
largest     <- image_effect %>% slice_max(image_effect_pts, n = 2)

cat("\n--- Image effect, Table B2 column ---\n")
print(as.data.frame(image_effect))
cat("\nVariables within +/- 1.3 pts : ", within_band, " of ", nrow(image_effect), "\n", sep = "")
cat("Two largest                  : ",
    paste0(largest$variable, " ", sprintf("%+.1f", largest$image_effect_pts), collapse = ", "),
    "\n", sep = "")
cat("\nWrote output/tables/study1_stability_image_effect.csv\n")


# =============================================================================
# Mean Fleiss's kappa by variable kind
#
# Closes the sentence in 3.2: "mean Fleiss's kappa reaching .81 across the nine
# binary frames with a defined kappa, .79 across the four multiclass variables,
# and .70 across the two visual ones". Those three means had no committed
# producer, although the per-variable kappas they average are in
# stability_results.csv.
#
# WHICH VARIABLES THE SENTENCE USES
# The next sentence in the thesis states the exclusion: "Four variables returned
# a single category or a near-zero prevalence on every pass, leaving only exact
# agreement interpretable." Those four are:
#   Normative, Language     one category on every pass, so kappa is undefined and
#                           the source file already carries an empty cell
#   Data_Visual, Youth_Style the two visual codes at near-zero prevalence, where
#                           exact agreement of .994 makes kappa uninformative in
#                           the same way
# The last two are named rather than derived from a prevalence cutoff, because no
# cutoff separates Youth style (1.6% of the corpus) from Interactivity (1.7%),
# which the sentence keeps among the binary nine. Naming them is honest about
# where the judgment sits; a tuned threshold would hide it.
#
# Both versions are written out, the reported one and the one over every defined
# kappa, so a reader can see what the exclusion costs instead of reconstructing
# it. Visual moves from .76 over four codes to .70 over the reported two.
# =============================================================================

kappa_excluded <- c("Data_Visual", "Youth_Style")

fleiss_all <- stab %>%
  filter(!is.na(kappa)) %>%
  group_by(kind) %>%
  summarise(n_all_defined    = n(),
            mean_kappa_all   = round(mean(kappa), 3), .groups = "drop")

fleiss_reported <- stab %>%
  filter(!is.na(kappa), !variable %in% kappa_excluded) %>%
  group_by(kind) %>%
  summarise(n_reported          = n(),
            mean_kappa_reported = round(mean(kappa), 3), .groups = "drop")

fleiss_by_kind <- left_join(fleiss_all, fleiss_reported, by = "kind") %>%
  dplyr::select(kind, n_reported, mean_kappa_reported, n_all_defined, mean_kappa_all)

write_csv(fleiss_by_kind, here("output/tables/study1_stability_fleiss_by_kind.csv"))
print(fleiss_by_kind)

# =============================================================================
# Same-coder between-session consistency
#
# Closes the sentence "the coder reproduced 260 of 334 cell entries, or 77.8%",
# reported as the ceiling on the blind coefficients. It was documented in
# data/study1/_provenance/Study1_ValidationSample_MERGE_AUDIT.md but generated by
# merge_blind_coding.py, which is not part of this package, so the figure had a
# documented origin and no runnable producer.
#
# This is same-coder consistency across two sessions, not an inter-rater
# statistic, and it enters no reliability estimate. It bounds them.
# =============================================================================

prov <- function(f) read_delim(here("data/study1/_provenance", f),
                               delim = ";", show_col_types = FALSE,
                               progress = FALSE)

coded_late  <- prov("Study1_ValidationSample_CODED (1).csv")   # pass A, later, complete
coded_early <- prov("Study1_ValidationSample_CODED.csv")       # pass B, earlier, partial

manual_cols <- names(coded_late)[startsWith(names(coded_late), "MANUAL_")]

as_long <- function(d) {
  d %>%
    dplyr::select(Post_ID, all_of(manual_cols)) %>%
    mutate(across(all_of(manual_cols), as.character)) %>%
    pivot_longer(-Post_ID, names_to = "variable", values_to = "code") %>%
    mutate(code = na_if(trimws(code), ""))
}

overlap <- inner_join(as_long(coded_late), as_long(coded_early),
                      by = c("Post_ID", "variable"), suffix = c("_late", "_early")) %>%
  filter(!is.na(code_late), !is.na(code_early))

coder_between_session <- tibble(
  posts_with_overlapping_cells = n_distinct(overlap$Post_ID),
  cells_filled_in_both         = nrow(overlap),
  cells_identical              = sum(overlap$code_late == overlap$code_early),
  pct_identical                = round(100 * mean(overlap$code_late == overlap$code_early), 1)
)

write_csv(coder_between_session, here("output/tables/study1_coder_between_session.csv"))
print(coder_between_session)
