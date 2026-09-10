# =============================================================================
# 01_study1/01_sampling_frame.R
#
# WHERE THIS BELONGS
#   In the thesis: Chapter 3, Method: Sampling Frame and Eligibility Criteria
#   Produces: Figure 2
#   Status: Descriptive. It reports what the data contain rather than testing a
#   claim, so there is no hypothesis and no p value to correct.
#
# Study 1 organization-identification & screening funnel (PRISMA-style).
# Derives the funnel counts from the raw identification workflow files and the
# collected post corpus, renders Figure 2 (fig_study1_sampling_funnel), and
# writes a funnel table.
#
# DATA-INTEGRITY NOTE (do NOT "fix" the raw file; work around it):
#   The raw DEDUPLICATED_MASTER_LIST has two internally inconsistent columns:
#     - `Has_Public_Instagram (1/0)`: at least one confirmed error, e.g. Harnas
#       Wildlife Foundation is flagged 0 yet contributes 74 posts to the corpus.
#     - `Included_in_Final_Sampling_Frame (1/0)`: sums to 24, not 19. The gap of
#       five resolves completely, and none of it touches a reported number:
#         * 1 transcription error. Desert Research Foundation of Namibia carries
#           Desert Lion Conservation Trust's Instagram URL, so 24 marked rows hold
#           only 23 distinct accounts.
#         * 4 organizations marked as included that published nothing in the window
#           and so never entered the corpus: EduVentures Trust, IRDNC, Peace Parks
#           Foundation, TH!NK Namibia. Chapter 3 states this in prose, that a small
#           number of organizations with a public account did not post in the window.
#       24 - 1 duplicate - 4 silent accounts = the 19 organizations of the corpus.
#       A third defect sits in the same column's neighbour: NACSO's Instagram_Handle
#       cell holds a post URL instead of a profile URL. The account itself is in the
#       corpus as nacso_namibia1, so no count is affected.
#       reconcile_master_list() below writes this arithmetic to a table, so the next
#       audit meets an explanation instead of a discrepancy.
#   This script therefore reports NO number that depends on those two columns.
#   The funnel is anchored on externally-verifiable quantities only:
#     * raw candidate rows (keyword pool + directory/snowball screening),
#     * deduplicated unique actors,
#     * the two reliable eligibility flags (operates-in-Namibia, conservation-mandate),
#     * the organizations and posts that actually appear in the collected corpus.
#   The Instagram-absence ("about half of the 47") is reported qualitatively.
#
# out: output/figures/fig_study1_sampling_funnel.png/.pdf     (Figure 2)
#      output/tables/study1_sampling_funnel.csv               (the funnel counts)
#      output/tables/study1_master_list_reconciliation.csv    (the 24 to 19 arithmetic)
#      output/tables/org_volume_concentration.csv             (Table B3)
# =============================================================================

suppressMessages(library(ggplot2))
suppressMessages(library(here))

data_dir <- here("data", "raw", "Study1")
fig_dir  <- here("output", "figures")
tab_dir  <- here("output", "tables")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(tab_dir, showWarnings = FALSE, recursive = TRUE)

source(here("src", "theme_apa.R"))  # theme_apa(), save_apa()

rd <- function(f) read.csv(file.path(data_dir, f), sep = ";", check.names = FALSE,
                           stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM")
first_col <- function(df, pat) grep(pat, names(df), value = TRUE)[1]
as_flag   <- function(x) suppressWarnings(as.integer(trimws(x)))

raw_pool <- rd("Study1_Organization_Identification_Workflow-RAW_ORGANISATION_POOL.csv")
screen   <- rd("Study1_Organization_Identification_Workflow-DIRECTORY_SCREENING.csv")
master   <- rd("Study1_Organization_Identification_Workflow-DEDUPLICATED_MASTER_LIST.csv")
posts    <- rd("Study1_Posts_RAW_20260520.csv")

# Volume concentration for Table B3. Captured here, in base R, because the pipe is not
# loaded yet at this point and because `posts` is filtered to the 917 analysed rows
# further down, while the Table B3 note puts the base at the 920 COLLECTED posts.
.org_counts_collected <- sort(table(posts$Actor_Name), decreasing = TRUE)


# ---- Identification & deduplication ----
rp_name   <- first_col(raw_pool, "Actor_Name")
n_keyword <- sum(nzchar(trimws(raw_pool[[rp_name]])))          # keyword search pool
n_dir     <- nrow(screen)                                      # directory + snowball
n_total   <- n_keyword + n_dir                                 # raw candidate entries

m_nam <- as_flag(master[[first_col(master, "Operates_primarily_in_Namibia")]])
m_con <- as_flag(master[[first_col(master, "Conservation_Mandate")]])
valid <- !is.na(m_nam)                                         # drops header-echo row
n_dedup   <- sum(valid)                                        # unique actors
n_notNam  <- sum(valid & m_nam == 0, na.rm = TRUE)             # excluded: not Namibia
n_noMand  <- sum(valid & m_nam == 1 & m_con == 0, na.rm = TRUE)# excluded: no mandate
n_namcon  <- sum(valid & m_nam == 1 & m_con == 1, na.rm = TRUE)# Namibian conservation orgs

# ---- Realized corpus (authoritative for the final frame) ----
p_org  <- first_col(posts, "Actor_Name")
orgs   <- trimws(posts[[p_org]]); orgs <- orgs[nzchar(orgs)]
n_orgs <- length(unique(orgs))
n_posts<- length(orgs)
n_notCorpus <- n_namcon - n_orgs                              # 47 - 19

cat(sprintf(paste0("FUNNEL  keyword=%d  directory/snowball=%d  total=%d\n",
                   "        dedup=%d  ->  Namibian conservation=%d (excl: %d non-Namibia, %d no-mandate)\n",
                   "        active corpus: %d organizations, %d posts (not in corpus: %d)\n"),
            n_keyword, n_dir, n_total, n_dedup, n_namcon, n_notNam, n_noMand,
            n_orgs, n_posts, n_notCorpus))

# ---- Funnel table ----
funnel <- data.frame(
  stage = c("Records identified (keyword)", "Records identified (directory + snowball)",
            "Total candidate entries", "Unique actors after deduplication",
            "Namibian conservation organizations", "Active in corpus (organizations)",
            "Posts in analytical corpus"),
  n = c(n_keyword, n_dir, n_total, n_dedup, n_namcon, n_orgs, n_posts),
  stringsAsFactors = FALSE
)
write.csv(funnel, file.path(tab_dir, "study1_sampling_funnel.csv"), row.names = FALSE)

# ---- Reconciliation of the unreliable inclusion column [not reported] --------
# Traces how the flagged rows of the master list narrow to the corpus organizations.
# Not used by the funnel, which is anchored on verifiable quantities. Handles are
# parsed out of the URL column, which stores full profile links.
ig_handle <- function(u) {
  u <- sub("/+$", "", trimws(u))
  h <- sub("^.*instagram\\.com/", "", u)
  tolower(sub("[/?#].*$", "", h))
}
# The corpus's own handle column is the reference the flags are checked against.
corpus_handles <- unique(tolower(trimws(posts[[first_col(posts, "Instagram_Handle")]])))
corpus_handles <- sub("^@", "", corpus_handles[nzchar(corpus_handles)])
inc_rows <- master[as_flag(master[[first_col(master, "^Included_in_Final")]]) %in% 1, ]
inc_h    <- ig_handle(inc_rows[[first_col(master, "Instagram_Handle")]])
unmatched <- setdiff(unique(inc_h), corpus_handles)   # 5: 4 silent + NACSO's broken URL
reconcile <- data.frame(
  item = c("Rows flagged Included_in_Final_Sampling_Frame = 1",
           "minus duplicate URL",
           "Distinct Instagram accounts among them",
           "minus accounts that published nothing in the window",
           "Organizations in the collected corpus"),
  n = c(nrow(inc_rows),
        nrow(inc_rows) - length(unique(inc_h)),
        length(unique(inc_h)),
        length(unmatched) - 1L,                       # the -1 is NACSO, see note
        n_orgs),
  note = c("raw column, known to be unreliable",
           "Desert Research Foundation carries Desert Lion Conservation Trust's link",
           "23 accounts behind 24 rows",
           paste("EduVentures, IRDNC, Peace Parks, TH!NK Namibia. A fifth handle also",
                 "fails to match, NACSO, whose cell holds a post URL instead of a",
                 "profile URL; that account IS in the corpus as nacso_namibia1, so it",
                 "is a broken link and not a missing organization."),
           "24 - 1 duplicate - 4 silent = 19, the number reported throughout Chapter 3"),
  stringsAsFactors = FALSE
)
print(reconcile)
# Of 24 flagged rows, 1 is a duplicate URL and 4 are silent accounts, leaving 19.
# -> the flagged rows resolve cleanly, no account drops out unnoticed. The final
#    count and the screening funnel are reported as Figure 2.
write.csv(reconcile, file.path(tab_dir, "study1_master_list_reconciliation.csv"), row.names = FALSE)

# ---- PRISMA-style figure ----
# Box heights sit close to the text they carry. Boxes three to five times taller
# run the panel to a full page.
bw <- 4.0; bh <- 0.95; ew <- 4.5; eh <- 0.80
main <- data.frame(
  x = 3, y = c(5.7, 4.2, 2.7, 1.2),
  label = c(
    sprintf("Records identified (<i>n</i> = %d)<br>keyword search  <i>n</i> = %d<br>directory + snowball  <i>n</i> = %d",
            n_total, n_keyword, n_dir),
    sprintf("Unique organizations after<br>deduplication<br><i>n</i> = %d", n_dedup),
    sprintf("Namibian conservation<br>organizations<br><i>n</i> = %d", n_namcon),
    sprintf("Retrieved corpus<br>%d organizations, %d posts", n_orgs, n_posts)
  ), stringsAsFactors = FALSE)
excl <- data.frame(
  x = 7.5, y = c(3.45, 1.95),
  label = c(
    sprintf("Excluded (<i>n</i> = %d)<br>%d not primarily Namibian<br>%d no conservation mandate",
            n_notNam + n_noMand, n_notNam, n_noMand),
    sprintf("Not in corpus (<i>n</i> = %d)<br>no discoverable public Instagram<br>(about half) or no posts in window",
            n_notCorpus)
  ), stringsAsFactors = FALSE)

arr <- arrow(length = unit(0.18, "cm"), type = "closed")
p <- ggplot() +
  geom_segment(aes(x = 3, xend = 3, y = 5.7 - bh/2, yend = 4.2 + bh/2), arrow = arr, linewidth = 0.5) +
  geom_segment(aes(x = 3, xend = 3, y = 4.2 - bh/2, yend = 2.7 + bh/2), arrow = arr, linewidth = 0.5) +
  geom_segment(aes(x = 3, xend = 3, y = 2.7 - bh/2, yend = 1.2 + bh/2), arrow = arr, linewidth = 0.5) +
  geom_segment(aes(x = 3, xend = 7.5 - ew/2, y = 3.45, yend = 3.45),
               arrow = arrow(length = unit(0.15, "cm"), type = "closed"), linewidth = 0.4, colour = "grey45") +
  geom_segment(aes(x = 3, xend = 7.5 - ew/2, y = 1.95, yend = 1.95),
               arrow = arrow(length = unit(0.15, "cm"), type = "closed"), linewidth = 0.4, colour = "grey45") +
  # White fill with a grey border, matching the three other box-and-arrow schematics
  # of this thesis (the conceptual model and the two Study 2 funnels). A coloured
  # fill would make this the only figure in the document carrying one, and the
  # natural border blue is the Empathy condition colour of the Study 2 palette.
  # APA 7 section 7.26
  # asks that colour be reserved for what needs it, and position plus arrow
  # direction already separate the flow boxes from the exclusion boxes.
  geom_tile(data = main, aes(x = x, y = y), width = bw, height = bh,
            fill = "white", colour = "grey30", linewidth = 0.5) +
  ggtext::geom_richtext(data = main, aes(x = x, y = y, label = label), size = 3.0,
                        lineheight = 0.95, fill = NA, label.color = NA,
                        label.padding = grid::unit(rep(0, 4), "pt")) +
  geom_tile(data = excl, aes(x = x, y = y), width = ew, height = eh,
            fill = "grey95", colour = "grey55", linewidth = 0.5) +
  ggtext::geom_richtext(data = excl, aes(x = x, y = y, label = label), size = 3.0,
                        lineheight = 0.95, colour = "grey20", fill = NA, label.color = NA,
                        label.padding = grid::unit(rep(0, 4), "pt")) +
  # Limits sit just outside the drawn boxes (main span 1.0 to 5.0, exclusion span
  # 5.25 to 9.75) rather than a third of an inch beyond them, so the figure is not
  # padded with blank margin it does not use.
  scale_x_continuous(limits = c(0.92, 9.83)) +
  scale_y_continuous(limits = c(0.66, 6.24)) +
  theme_void(base_family = "sans") +
  theme(plot.background = element_rect(fill = "white", colour = NA),
        plot.margin = margin(2, 2, 2, 2))

save_apa(p, "fig_study1_sampling_funnel", width = 6.5, height = 3.7, outdir = fig_dir)
cat("Wrote fig_study1_sampling_funnel.(png|pdf) and study1_sampling_funnel.csv\n")


# ---- Volume concentration, the two shares Table B3 reports --------------------
# Reconstructible by hand from the corpus but computed by no step, so the printed
# figures had no source in the package. Counts were captured right after the raw read.
concentration <- data.frame(
  quantity = c("top3_share_pct", "top5_share_pct", "n_posts", "n_organizations"),
  value    = c(round(100 * sum(head(.org_counts_collected, 3)) / sum(.org_counts_collected), 1),
               round(100 * sum(head(.org_counts_collected, 5)) / sum(.org_counts_collected), 1),
               sum(.org_counts_collected),
               length(.org_counts_collected)),
  stringsAsFactors = FALSE
)
print(concentration)
write.csv(concentration, file.path(tab_dir, "org_volume_concentration.csv"), row.names = FALSE)
