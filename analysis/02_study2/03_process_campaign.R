# =============================================================================
# 02_study2/03_process_campaign.R
#
# WHERE THIS BELONGS
#   In the thesis: Chapter 4, Method: Campaign Structure and Measures
#   Produces: no object, it prepares the campaign data
#   Status: No test. It prepares or draws, and computes no result.
#
# Study 2 - Naturalistic Framing Comparison: raw -> processed data
#
# Turns the three Study 2 raw sources into the clean files 02_study2/04_confirmatory.R consumes.
# Study 2 is multi-round (two ad flights) and multi-source, so each source has its
# own guarded block: a missing source is skipped with a message and the rest still
# run (e.g. Meta round 1 can be processed before GA4 / the exit survey exist).
#
# Inputs (data/raw/):
#   MetaAds/MetaAds_export_<round>_*.csv   one export per round   -> meta_ads_clean.csv   (H1, H3, H4, H7, RQ1)
#   GA4/*.csv                              GA4 free-form exports  -> ga4_clean.csv        (H5)
#   ExitSurvey/**/data_*.csv               SoSci data export      -> exitsurvey_clean.csv (H2a, H2b, H6)
#
# Outputs (data/processed/):
#   meta_ads_clean.csv     one row per ad = condition x variant x round
#   ga4_clean.csv          one row per landing-page UTM cell (derived session counts)
#   exitsurvey_clean.csv   one row per exit-survey respondent with a valid condition
#
# Run order: 02_study2/03_process_campaign.R -> 02_study2/04_confirmatory.R
# =============================================================================

# pacman::p_load() installs a package if missing, then loads it, in one call.
pacman::p_load(here, tidyverse)

# Source every helper in src/ (theme_apa(), condition_colours, save_apa(),
# save_apa_table(), fmt_* formatters). walk() = a for-loop that runs source() on
# each file for its side effect and returns nothing. This script only writes
# processed data, but sourcing keeps it consistent with the rest of the pipeline.
list.files(here("src"), full.names = TRUE) %>% walk(source)


# ---- Analysis-wide constants ------------------------------------------------

# Canonical condition order, reused in every block. Neutral FIRST so it stays the
# reference level downstream (the control every motivational frame is compared to).
cond_levels <- c("Neutral", "Empathy", "Social Norm", "Narrative")

# Maps the utm_content prefix (how conditions travel through GA4 / SoSci links) to
# the display label. Reused by the GA4 and exit-survey blocks.
utm_to_condition <- c(
  neutral = "Neutral", empathy = "Empathy",
  social_norm = "Social Norm", narrative = "Narrative"
)

# The flight R1 ended on this date; anything later belongs to round 2. Used to
# stamp GA4 and exit-survey rows (both exports accumulate BOTH rounds in one file).
flight_r1_end <- as.Date("2026-06-23")

dir.create(here("data/processed"), showWarnings = FALSE, recursive = TRUE)   # ensure output folder exists (quietly)


# ---- 1. Meta Ads: raw -> processed ------------------------------------------
# Reads every MetaAds_export_*.csv in data/raw/MetaAds/ (one per round), row-binds
# them, and writes the processed file 02_study2/04_confirmatory.R consumes. All columns (incl. the
# exploratory engagement metrics) are carried through unchanged.

meta_files <- list.files(
  here("data/raw/MetaAds"),
  pattern    = "^MetaAds_export_.*\\.csv$",   # one export per campaign round
  full.names = TRUE
)

if (length(meta_files) == 0) {
  message("No MetaAds_export_*.csv in data/raw/MetaAds/ - skipping Meta processing.")
} else {
  meta_ads_clean <- meta_files %>%
    map_dfr(~ read_csv(.x, show_col_types = FALSE)) %>%   # read each round's CSV, row-bind into one table
    mutate(
      condition = factor(condition, levels = cond_levels),   # enforce the canonical condition order
      round     = factor(round)                              # round is a grouping label, not a quantity
    )

  write_csv(meta_ads_clean, here("data/processed/meta_ads_clean.csv"))

  nrow(meta_ads_clean)
  # 24 ad rows written (12 ads x 2 rounds) -> data/processed/meta_ads_clean.csv.
}


# ---- 2. GA4: raw -> processed -----------------------------------------------
# Source: GA4 Explorations free-form CSV export (one per round), aggregated by
# Session manual ad content (= utm_content) x geography. Format quirks handled:
#   - 5 "#" comment lines + 1 blank line before the header (line 4 = date range,
#     used to assign the round: flight R1 ended 2026-06-23, R2 = 06-24..07-07)
#   - a Grand-total row with an extra trailing field (dropped via the utm filter)
#   - column ORDER differs between exports, and R2 adds "Returning users", so
#     columns are matched by NAME, not position
#   - rows with a non-empty "Link URL" are event-scoped breakouts of sessions
#     already counted in the empty-Link-URL row of the same utm cell; keeping them
#     would double-count session metrics -> dropped
#   - the 2026-07-30 re-exports carry a measured `Sessions` column and are used
#     directly. The older exports had none, so sessions had to be DERIVED as
#     user_engagement / avg_engagement_time_per_session with a first_visits
#     fallback; that path is kept for any export lacking the column. The old
#     files also used GA4's "Last 28 days" default window, which missed the
#     final day of Round 1 and swept in pre-launch link tests; they now sit in
#     data/raw/GA4/_superseded/.
# Target schema (H5 summary mode): condition + sessions + avg_session_duration_s
# (+ geo / diagnostic columns).

ga4_files <- list.files(here("data/raw/GA4"), pattern = "\\.csv$", full.names = TRUE)

if (length(ga4_files) == 0) {
  message("No GA4 export in data/raw/GA4/ - skipping GA4 processing (H5 skipped downstream).")
} else {

  # Parse ONE GA4 export into tidy rows. Returns NULL for unsupported (segmented)
  # exports so map_dfr silently drops them while other files still process.
  parse_ga4_export <- function(path) {
    head_lines <- read_lines(path, n_max = 8)   # peek at the pre-header rows GA4 prepends

    # Segmented exports repeat every metric per segment (All Users / Mobile / ...)
    # in an extra pre-header row; parsing them would double-count sessions. Only
    # un-segmented exports are supported.
    if (any(str_detect(head_lines, "^,.*Segment"))) {
      message("Skipping segmented GA4 export: ", basename(path))
      return(NULL)   # bail out of THIS file; map_dfr drops the NULL
    }

    # Line 4 carries the date range as YYYYMMDD-YYYYMMDD. Take the END date
    # (characters 10-17) and use it to label the round.
    date_range <- head_lines[4] %>% str_extract("[0-9]{8}-[0-9]{8}")
    flight_end <- as.Date(str_sub(date_range, 10, 17), format = "%Y%m%d")
    round_id   <- if_else(flight_end <= flight_r1_end, 1L, 2L)   # <= 23 Jun -> R1, later -> R2

    suppressWarnings(read_csv(path, skip = 6, show_col_types = FALSE)) %>%   # skip the 6 pre-header rows
      rename_with(~ str_to_lower(.x) %>%                     # tidy column names: lower-case...
                    str_replace_all("[^a-z0-9]+", "_")) %>%  # ...and collapse any non-alphanumerics to "_"
      # The Link URL dimension is only present in exports that requested it. When
      # it is there, event-scoped breakout rows must go or sessions double-count;
      # when it is absent there are no breakout rows to drop.
      { if ("link_url" %in% names(.)) filter(., is.na(link_url)) else . } %>%
      filter(
        str_detect(session_manual_ad_content,               # keep only genuine ad utm cells (condition_variant)
                   "^(neutral|empathy|social_norm|narrative)_[0-9]+$")
      ) %>%
      mutate(round = round_id)   # stamp every kept row with the round derived above
  }

  ga4_clean <- ga4_files %>%
    map_dfr(parse_ga4_export) %>%   # parse every GA4 file and row-bind (skipped/NULL files just vanish)
    mutate(
      utm_content = session_manual_ad_content,   # e.g. "social_norm_2"
      # str_remove strips the "_variant" suffix; the lookup maps the prefix to a
      # label; unname() drops the named-vector names so factor() gets a plain vector.
      condition   = factor(
        unname(utm_to_condition[str_remove(utm_content, "_[0-9]+$")]),
        levels = cond_levels
      ),
      variant  = as.integer(str_extract(utm_content, "[0-9]+$")),   # trailing number = which of the 3 variants
      # Use the measured session count when the export carries one. The derivation
      # below is the legacy path for exports without it and is kept only so old
      # files remain parseable; it approximates sessions as
      # user_engagement / avg engagement time per session, falling back to
      # first_visits when nothing engaged.
      sessions = if ("sessions" %in% names(.)) as.integer(sessions) else as.integer(if_else(
        average_engagement_time_per_session > 0,
        round(user_engagement / average_engagement_time_per_session),
        pmax(first_visits, 1)   # pmax = element-wise max, floors the fallback at 1
      )),
      # Derive the per-session engagement time when the export omits it, so the
      # downstream schema stays identical across export vintages.
      average_engagement_time_per_session = if ("average_engagement_time_per_session" %in% names(.))
        average_engagement_time_per_session else if_else(sessions > 0, user_engagement / sessions, 0),
      first_visits = if ("first_visits" %in% names(.)) first_visits else NA_integer_,
      round = factor(round)
    ) %>%
    dplyr::select(   # keep + rename to the schema 02_study2/04_confirmatory.R expects (the *_s suffixes flag seconds)
      # City and Region are dropped here: the export resolves them to single
      # sessions in some cells, which no analysis uses and which would identify
      # individual visitors. Country is coarse enough to keep.
      round, condition, variant, utm_content, country,
      sessions,
      avg_session_duration_s = average_session_duration,
      avg_engagement_time_s  = average_engagement_time_per_session,
      engagement_rate, first_visits,
      user_engagement_s = user_engagement
    )

  write_csv(ga4_clean, here("data/processed/ga4_clean.csv"))

  c(utm_rows = nrow(ga4_clean), sessions = sum(ga4_clean$sessions))
  # 2026-07-30 re-export, measured Sessions column, campaign-exact windows:
  # 62 UTM-attributed sessions (Round 1 = 37, Round 2 = 25).
  # The previous figure was 66 (40 + 25/26) from the defective-window exports.
}


# ---- 3. Exit survey (SoSci): raw -> processed -------------------------------
# Source: SoSci exit-survey export. Condition travels in via the UTM passthrough
# (hidden CONDITION set from utm_content). Target schema for exitsurvey_clean.csv:
#   condition   (from the ?r= link reference REF, e.g. "social_norm_2")
#   gender      (H2a/H2b, Empathy)  age_group (descriptive)    env_concern (H6; numeric)
#   + exploratory items (empathic concern, perspective-taking, biospheric, collectivism)
# H2a, H2b and H6 are only tested at N >= 20 (guard lives in 02_study2/04_confirmatory.R).
# SoSci layout (from the variables file): DM01 = age, DM02 = gender,
# IM01_03 = environmental concern. -9 = not answered / -1 = not shown.
# The data_*.csv is tab-separated, UTF-16LE (never values_/variables_/codebook_).
# SoSci accumulates, so a later export repeats the responses of an earlier one. Every
# data_*.csv is therefore read and the union is de-duplicated on CASE, the response id
# SoSci assigns. Selecting one file instead would have to decide which is newest, and
# the only two candidates for that are the filesystem modification time and the export
# timestamp in the filename. Modification time does not survive a copy: unpacking a ZIP
# or cloning the repository stamps every file at the same instant, and the tie then
# resolves by list order, which silently picks the smaller export. Reading all of them
# needs no such decision and stays correct even if one export covers only part of the
# field period.

exitsurvey_files <- list.files(
  here("data/raw/ExitSurvey"),
  pattern = "^data_.*\\.csv$", recursive = TRUE, full.names = TRUE   # only data_* files (recurse into session subfolders)
)

if (length(exitsurvey_files) == 0) {
  message("No SoSci data_*.csv in data/raw/ExitSurvey/ - skipping exit-survey processing (H2a/H2b/H6 skipped downstream).")
} else {
  raw <- exitsurvey_files %>%
    map_dfr(~ read_delim(
      .x, delim = "\t",                        # SoSci: tab-separated...
      locale = locale(encoding = "UTF-16LE"),   # ...UTF-16LE encoded
      show_col_types = FALSE
    )) %>%
    distinct(CASE, .keep_all = TRUE)            # union of all exports, one row per response
  # 14 responses across the two exports, 10 of which the first export already carried.

  # SoSci records non-response as negative codes (-9 = not answered, -1 = not
  # shown); turn any negative into NA so it never enters a mean or correlation.
  neg_na <- function(x) {
    x <- suppressWarnings(as.numeric(x))   # text -> number (blank/odd values become NA; warning silenced)
    if_else(x < 0, NA_real_, x)            # SoSci codes missing as -9 / -1
  }

  exitsurvey_clean <- raw %>%
    transmute(   # transmute = mutate + keep only these columns
      case        = CASE,
      ref         = REF,                                                 # the ?r= link reference = utm_content, e.g. "social_norm_2"
      condition   = factor(unname(utm_to_condition[str_remove(REF, "_[0-9]+$")]),   # strip "_variant" -> condition label
                           levels = cond_levels),
      variant     = as.integer(str_extract(REF, "[0-9]+$")),             # trailing number = variant 1/2/3
      started     = STARTED,
      # Round via response date (R1 flight ended 2026-06-23; the export spans both).
      round       = factor(if_else(as.Date(STARTED) <= flight_r1_end, 1L, 2L)),
      gender      = neg_na(DM02),       # H2a, H2b
      age_group   = neg_na(DM01),       # descriptive
      emp_state   = neg_na(IM01_01),    # state empathic concern (exploratory)
      ident_state = neg_na(IM01_02),    # perspective-taking (exploratory)
      env_concern = neg_na(IM01_03),    # H6 (concern for Namibia's environment)
      bio_water   = neg_na(IM01_04),    # biospheric value, water (H6.2)
      collectivism = neg_na(IM01_05)    # item 5, descriptive only (community well-being)
    ) %>%
    filter(!is.na(condition))   # drop direct/test visits with no ad condition

  write_csv(exitsurvey_clean, here("data/processed/exitsurvey_clean.csv"))

  nrow(exitsurvey_clean)
  # 13 responses with a valid condition -> data/processed/exitsurvey_clean.csv
  # (below the N = 20 gate, so H2a, H2b and H6 are not tested downstream).
}
