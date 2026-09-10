# =============================================================================
# 02_study2/01_process_pretest.R
#
# WHERE THIS BELONGS
#   In the thesis: Chapter 4, Method: Stimulus Pretest
#   Produces: no object, it prepares the pretest data
#   Status: No test. It prepares or draws, and computes no result.
#
# Study 2 pretest - raw SoSci export to processed, analysis-ready tables.
#
# Reads the SoSci Survey export of the within-subjects pretest, keeps valid
# cases, reshapes the 4 x 11 rating grid into one long item-level table, builds
# composite subscale scores, and writes the four processed files 02_study2/02_pretest_validation.R
# consumes. The raw file on disk is never modified.
#
# Inputs  (data/raw/Pretest/):
#   data_digital-altruism-pretest_*.csv   SoSci export (UTF-16LE, tab, comma decimals)
#
# Outputs (data/processed/):
#   pretest_items_long.csv    one row per participant x message x item
#   pretest_composites.csv    one row per participant x message x subscale
#   pretest_demographics.csv  one row per participant (age, gender, English)
#   pretest_order.csv         one row per participant x presentation slot
#
# Run order: 02_study2/01_process_pretest.R -> 02_study2/02_pretest_validation.R
# Study 2 (Meta Ads + GA4 + exit survey) is processed separately in 01b; this
# file handles the pretest only.
# =============================================================================

# pacman::p_load() installs a package if missing, then loads it, in one call.
pacman::p_load(here, tidyverse, janitor, flextable, officer)

# Source every helper in src/ (theme_apa(), condition_colours, save_apa(),
# theme_flex_apa(), save_apa_table(), fmt_* formatters). walk() = a for-loop that
# runs source() on each file for its side effect and returns nothing.
list.files(here("src"), full.names = TRUE) %>% walk(source)


# ---- Analysis-wide constants ------------------------------------------------

# Neutral listed FIRST so it becomes the factor reference level, the control the
# three motivational messages are compared against.
cond_levels  <- c("Neutral", "Empathy", "Social Norm", "Narrative")
scale_levels <- c("EMP", "NORM", "NAR", "CTRL1", "CTRL2")

# SoSci block codes -> experimental condition. Each participant rated all four
# messages (within-subjects); the four 11-item blocks live under these codes.
block_to_condition <- c(
  "EM01" = "Empathy",       # Message B in the setup doc
  "NA03" = "Narrative",     # Message D
  "NT03" = "Neutral",       # Message A
  "SO04" = "Social Norm"    # Message C
)


# ---- Locate the SoSci export ------------------------------------------------
# SoSci writes a fresh timestamped file on every export, so the LATEST match in
# data/raw/ is picked rather than a hard-coded filename. The export timestamp is
# read from the filename itself (YYYY-MM-DD_HH-MM), not from the file's modified
# time, because copying the folder does not preserve modification times.

sosci_files <- list.files(
  path       = here("data/raw"),
  pattern    = "^data_digital-altruism-pretest_.*\\.csv$",   # .* absorbs the timestamp in the name
  recursive  = TRUE,      # exports live in data/raw/Pretest/, not data/raw/ itself
  full.names = TRUE       # full paths so read_delim() can open them
)

# str_extract() pulls the "2026-06-13_16-18" stamp from each name; ymd_hm() parses
# it to a datetime; which.max() gives the index of the latest export.
export_time <- sosci_files %>%
  basename() %>%
  str_extract("\\d{4}-\\d{2}-\\d{2}_\\d{2}-\\d{2}") %>%
  lubridate::ymd_hm()
sosci_path <- sosci_files[which.max(export_time)]

basename(sosci_path)
# "data_digital-altruism-pretest_2026-06-13_16-18.csv" is the export the rest of the
# script reads, the 31 recorded cases from which 24 complete ones are retained. It is
# the only one in the package. Earlier partial downloads of the same collection were
# not kept, and neither was a single response from before the instrument was revised
# on 26 May, which used a different consent text, response scale and variable scheme.


# ---- Load raw data ----------------------------------------------------------
# SoSci export settings (from the download dialog): UTF-16LE encoding, tab
# delimiter, German comma decimals, and missing codes -1 (item not shown) / -9
# (not answered). read_delim() handles all of this in one call.

raw <- read_delim(
  file           = sosci_path,
  delim          = "\t",                                              # tab-separated (SoSci default)
  locale         = locale(encoding = "UTF-16LE", decimal_mark = ","), # UTF-16LE text + comma decimals
  na             = c("", "NA", "-1", "-9"),   # read these AS missing (-1 = not shown, -9 = not answered)
  show_col_types = FALSE
)

dim(raw)
# 31 rows x 77 columns in the raw export (all started cases, every SoSci column).


# ---- Keep only valid responses ----------------------------------------------
# FINISHED == 1 (reached the last page) AND STATUS == "complete" (SoSci's own
# valid-case marker) together give a conservative valid-case definition.

valid <- raw %>%
  filter(FINISHED == 1, STATUS == "complete")

nrow(valid)
# 24 valid, complete responses out of the 31 started cases.


# ---- Reshape to long item-level format --------------------------------------
# One row per participant x message x item. A single pivot_longer captures the
# SoSci block code (EM01/NA03/NT03/SO04) and the two-digit item number; condition
# and subscale are then derived from the parsed pieces. Within each 11-item block
# items map to subscales, readable in materials/pretest/instrument.md: 1-3 EMP, 4-6 NORM,
# 7-9 NAR, 10 CTRL1 (persuasiveness), 11 CTRL2 (comprehension).

# Map an item number (1-11) to its subscale. case_when() = vectorised if/else:
# the FIRST matching row (top to bottom) wins.
item_to_scale <- function(item_num) {
  case_when(
    item_num %in% 1:3 ~ "EMP",     # items 1-3  = Empathic Concern
    item_num %in% 4:6 ~ "NORM",    # items 4-6  = Descriptive Norm
    item_num %in% 7:9 ~ "NAR",     # items 7-9  = Narrativity
    item_num == 10    ~ "CTRL1",   # item 10    = Persuasiveness control
    item_num == 11    ~ "CTRL2"    # item 11    = Comprehension control
  )
}

items_long <- valid %>%
  dplyr::select(
    id = CASE,                                   # rename SoSci's CASE to a tidy id
    matches("^(EM01|NA03|NT03|SO04)_\\d{2}$")    # keep only the 4x11 rating columns
  ) %>%
  # Wide -> long: turn the 44 rating columns into one row per participant x item.
  pivot_longer(
    cols          = -id,                             # pivot every column except id
    names_to      = c("block", "item_num"),          # split each column name into two captured pieces...
    names_pattern = "([A-Z]{2}\\d{2})_(\\d{2})",     # ...group 1 = block, group 2 = item number
    values_to     = "score"
  ) %>%
  mutate(
    item_num  = as.integer(item_num),               # "01" (text) -> 1 (number)
    # block_to_condition[block] looks each code up in the named vector -> label.
    condition = factor(block_to_condition[block], levels = cond_levels),
    scale     = factor(item_to_scale(item_num), levels = scale_levels),
    item_id   = paste0(scale, "_", item_num)         # e.g. "EMP_1", human-readable
  ) %>%
  filter(!is.na(score)) %>%                          # drop items never answered (-1/-9 became NA)
  dplyr::select(id, condition, scale, item_num, item_id, score)   # final tidy column order

nrow(items_long)
# 1056 item-level rows = 24 participants x 4 messages x 11 items (no missing cells).


# ---- Composite scores (participant x condition x scale) ---------------------
# Target subscales (EMP, NORM, NAR) are means of three items; the two control
# items (CTRL1, CTRL2) are single items but kept in the same long shape so
# downstream code treats all five scales uniformly.

composites <- items_long %>%
  group_by(id, condition, scale) %>%          # one group per participant x message x subscale
  summarise(
    # The count is taken FIRST. summarise() evaluates in order, so assigning `score`
    # before counting would make the count refer to the new scalar, not to the items.
    n_items = sum(!is.na(score)),             # how many items actually contributed (audit trail)
    score   = mean(score, na.rm = TRUE),      # subscale score = mean of its items (3 for EMP/NORM/NAR, 1 for controls)
    .groups = "drop"                          # ungroup after so later code is not silently grouped
  )

nrow(composites)
# 480 composite rows = 24 participants x 4 messages x 5 subscales.


# ---- Demographics -----------------------------------------------------------
# Labels taken verbatim from the SoSci values codebook. As-built page has THREE
# items: DM01 = AGE (4 brackets), DM02 = GENDER (1 = Woman, 2 = Man - do NOT
# assume 1 = Man), DM03 = ENGLISH proficiency (ordinal 1-5). The original
# "Namibia connection" item was dropped 2026-06-10, so DM03 now captures English
# proficiency, NOT a Namibia connection.

demographics <- valid %>%
  transmute(                                   # transmute = mutate but keep ONLY the columns built here
    id           = CASE,
    age_group    = factor(
      DM01, levels = 1:4,
      labels = c("18–24", "25–29", "30–35", "36 or older")   # numeric codes 1-4 -> readable brackets
    ),
    gender       = factor(
      DM02, levels = 1:5,
      labels = c("Woman", "Man", "Non-binary",               # 1 = Woman, 2 = Man (SoSci codebook order)
                 "Prefer not to say", "Prefer to self-describe")
    ),
    english_prof = factor(
      DM03, levels = 1:5,
      labels  = c("Basic", "Limited", "Intermediate", "Advanced", "Fluent / Native"),
      ordered = TRUE                           # ordinal: Basic < Limited < ... < Fluent (order carries meaning)
    )
  )

count(demographics, gender)
# 12 Woman and 12 Man (evenly split); no other gender codes appear in this sample.


# ---- Presentation order (for the order-effect check in 02_study2/02_pretest_validation.R) --------
# RG01x01..x04 store the four stimulus page labels in the order each participant
# saw them. Reshaped to one row per participant x slot so 02 can cross-tabulate
# position against message.

if (all(c("RG01x01", "RG01x02", "RG01x03", "RG01x04") %in% names(valid))) {   # only if all 4 slot columns exist
  order_data <- valid %>%
    dplyr::select(id = CASE, RG01x01, RG01x02, RG01x03, RG01x04) %>%
    pivot_longer(
      cols      = starts_with("RG01x"),        # the 4 slot columns -> long
      names_to  = "position",                  # column name encodes the slot (RG01x01 = slot 1)
      values_to = "page_label"
    ) %>%
    # str_extract("\\d+$") pulls the trailing digits: "RG01x03" -> "3" -> 3.
    mutate(position = as.integer(str_extract(position, "\\d+$")))
} else {
  # Older exports without the Random Generator output: emit an empty placeholder
  # so 02_study2/02_pretest_validation.R detects the absence and skips the order-effect check.
  order_data <- tibble(id = integer(), position = integer(), page_label = character())
  warning("RG01x01..x04 columns not found - order-effect check will be skipped.")
}

nrow(order_data)
# 96 rows = 24 participants x 4 presentation slots; the RG01 log is present.


# ---- Write processed files --------------------------------------------------
# 02_study2/02_pretest_validation.R reads only from data/processed/. Four separate files (items,
# composites, demographics, order) keep merging concerns out of the analysis.

dir.create(here("data/processed"), showWarnings = FALSE, recursive = TRUE)

write_csv(items_long,   here("data/processed/pretest_items_long.csv"))
write_csv(composites,   here("data/processed/pretest_composites.csv"))
write_csv(demographics, here("data/processed/pretest_demographics.csv"))
write_csv(order_data,   here("data/processed/pretest_order.csv"))

c(participants = n_distinct(items_long$id),
  item_rows    = nrow(items_long),
  composites   = nrow(composites))
# participants = 24, item_rows = 1056, composites = 480. Four CSVs written to
# data/processed/, ready for 02_study2/02_pretest_validation.R.
