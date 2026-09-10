# ---------------------------------------------------------------------------
#
# WHERE THIS BELONGS
#   In the thesis: Chapter 4, Results: Exploratory Analyses
#   Produces: Figure 9
#   Status: Exploratory. Study 2 has a preregistration, but this script is not
#   part of it. Only 04_confirmatory.R and 05_confirmatory_h2a.R are, and any
#   result here is a finding to be retested rather than a test.
#
# 02_study2/08_delivery_funnel.R
# The realized delivery funnel, Study 2.
#
# Deliberately repeats the layout of 00_figures_conceptual.R. The Method
# figure shows the chain and how each stage is measured; this one shows the
# same chain with the counts it actually produced. A reader meets the same
# shape twice, once as a plan and once as an outcome.
#
# One line, pooled across conditions, and deliberately so. Drawing a line per
# condition would promise a between-condition comparison the data do not carry.
# Up to the link click the four lines sit on top of each other, and the fan that
# opens at the last two stages rests on 10 to 18 sessions per condition. The
# condition-level counts belong in Table F4 and in the engagement models, where
# their uncertainty is visible.
#
# Input : output/tables/study2_expl_engagement_funnel.csv
# Output: output/figures/fig_study2_engagement_funnel.{png,pdf}
# ---------------------------------------------------------------------------
library(ggplot2); library(dplyr); library(readr); library(here)
source(here("src", "theme_apa.R"))

# ---- 1. The realized counts at each stage ------------------------------------
fn <- read_csv(here("output/tables/study2_expl_engagement_funnel.csv"),
               show_col_types = FALSE)
# The exit survey closes the chain. Its N comes from the pooled item table, where
# every row carries the same count of valid condition-tagged responses.
ex <- read_csv(here("output/tables/study2_expl_exitsurvey_pooled.csv"),
               show_col_types = FALSE)

tot <- fn %>% summarise(across(c(impressions, views_3s, link_clicks,
                                 sessions, engaged_sessions), sum))
n_exit <- max(ex$n)

# The realized chain: 430,840 impressions produced 81,504 three-second views
# (18.9%), 1,887 link clicks (2.3% of those views), 62 landing-page sessions
# (3.3% of the clicks) and 13 exit-survey responses (21.0% of the sessions).
# The drop at the session stage is the tracking shortfall the Limitations
# describe, since analytics loaded only for consenting visitors.
cnt <- c(tot$impressions, tot$views_3s, tot$link_clicks, tot$sessions, n_exit)
# Each label wraps where the box would otherwise be too narrow at the 8-pt floor,
# the same device 10_comment_reception.R uses.
lab <- c("Impressions", "3-s video\nviews", "Link clicks",
         "Landing-page\nsessions", "Exit survey")
# Share of the preceding stage, the quantity a funnel is read for. The figure
# note says what the percentage is a share of, so the boxes carry the bare value
# and stay inside their borders at the 8-pt floor.
pct <- c(NA, cnt[2] / cnt[1], cnt[3] / cnt[2], cnt[4] / cnt[3], cnt[5] / cnt[4])
sub <- ifelse(is.na(pct), format(cnt, big.mark = ",", trim = TRUE),
              sprintf("%s\n(%.1f%%)", format(cnt, big.mark = ",", trim = TRUE), 100 * pct))


# ---- 2. Box geometry, laid out to match the Method figure ---------------------
# Same five columns as 00_figures_conceptual.R, so the plan and the outcome
# stack stage for stage.
fx <- data.frame(
  xmin = c(0.30, 2.30, 4.30, 6.30, 8.75),
  xmax = c(2.00, 4.00, 6.00, 8.45, 10.80),
  ymin = rep(3.05, 5), ymax = rep(4.20, 5), lab = lab, sub = sub
)

eng <- tot$engaged_sessions
eng_sub <- sprintf("%d\n(%.0f%% of sessions)", eng, 100 * eng / cnt[4])

p <- ggplot() +
  geom_rect(data = fx, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "white", colour = "grey30", linewidth = 0.4) +
  geom_rect(data = data.frame(xmin = 6.30, xmax = 8.45, ymin = 1.10, ymax = 2.20),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "white", colour = "grey30", linewidth = 0.4) +
  geom_segment(data = data.frame(x = c(2.00, 4.00, 6.00, 8.45),
                                 xend = c(2.30, 4.30, 6.30, 8.75),
                                 y = rep(3.63, 4), yend = rep(3.63, 4)),
               aes(x = x, xend = xend, y = y, yend = yend),
               arrow = arrow(length = unit(0.14, "cm"), type = "closed"),
               colour = "grey15", linewidth = 0.42) +
  annotate("segment", x = 7.38, xend = 7.38, y = 3.05, yend = 2.20,
           arrow = arrow(length = unit(0.14, "cm"), type = "closed"),
           colour = "grey15", linewidth = 0.42) +
  geom_text(data = fx, aes(x = (xmin + xmax) / 2, y = 3.92, label = lab),
            size = 3.0, fontface = "bold", colour = "grey10", lineheight = 1.15) +
  geom_text(data = fx, aes(x = (xmin + xmax) / 2, y = 3.30, label = sub),
            size = 3.0, colour = "grey45", lineheight = 1.15) +
  annotate("text", x = 7.38, y = 1.86, label = "Engaged sessions",
           size = 3.0, fontface = "bold", colour = "grey10") +
  annotate("text", x = 7.38, y = 1.44, label = eng_sub,
           size = 3.0, colour = "grey45", lineheight = 1.15) +
  annotate("segment", x = 1.15, xend = 1.15, y = 3.05, yend = 2.45, colour = "grey55", linewidth = 0.3) +
  annotate("segment", x = 5.15, xend = 5.15, y = 3.05, yend = 2.45, colour = "grey55", linewidth = 0.3) +
  annotate("segment", x = 1.15, xend = 5.15, y = 2.45, yend = 2.45, colour = "grey55", linewidth = 0.3) +
  annotate("text", x = 3.15, y = 2.08,
           label = sprintf("click-through rate, the primary outcome\n%s of %s impressions, %.2f%%",
                           format(cnt[3], big.mark = ","), format(cnt[1], big.mark = ","),
                           100 * cnt[3] / cnt[1]),
           size = 3.0, colour = "grey20", lineheight = 1.2) +
  coord_cartesian(xlim = c(0.15, 10.95), ylim = c(0.98, 4.36), expand = FALSE) +
  labs(x = NULL, y = NULL) + theme_apa() +
  theme(plot.margin = margin(3, 4, 3, 3),
        axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.line = element_blank(),
        panel.border = element_blank())

save_apa(p, "fig_study2_engagement_funnel", width = 6.5, height = 2.25)
cat(sprintf("delivery funnel written: %s impressions -> %d exit-survey responses (1 in %s)\n",
            format(cnt[1], big.mark = ","), cnt[5],
            format(round(cnt[1] / cnt[5]), big.mark = ",")))
