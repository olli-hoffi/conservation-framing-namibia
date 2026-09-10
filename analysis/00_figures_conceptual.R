# =============================================================================
# 00_figures_conceptual.R
#
# WHERE THIS BELONGS
#   In the thesis: Chapter 2, Synthesis and Hypotheses; Chapter 4, Measures
#   Produces: Figures 1 and 5
#   Status: No test. It prepares or draws, and computes no result.
#
# The two schematic figures of the thesis. Neither computes a result. Both draw
# a fixed diagram, so they depend on no data file and can run at any point.
#
#   Figure 1  the conceptual model, how the three framings are expected to act
#   Figure 5  the measurement chain, impression to landing page to exit survey
#
# The funnel carries no realized percentage. That would preview results inside
# the Method section, which APA 7 section 3.4 does not permit. The companion
# figure fig_study2_engagement_funnel repeats the layout with the numbers, and
# it belongs to the Results.
#
# Output: output/figures/fig_conceptual_model.{png,pdf}
#         output/figures/fig_measurement_funnel.{png,pdf}
# =============================================================================


# here() anchors every path to the project root, so the script runs from any
# working directory. save_apa() and theme_apa() come from src/.
pacman::p_load(here, ggplot2, dplyr)
source(here("src", "theme_apa.R"))


# ---- Figure 1, the conceptual model -----------------------------------------

# APA 7.26 sets an 8-pt floor for text inside a figure image. build.js scales EVERY
# embedded figure to a 6.2-in column, so a figure saved at 7.4 in is reduced by 0.838
# and its type shrinks with it. At the original 2.35 to 3.0 mm these labels rendered at
# 5.60 to 7.15 pt in the docx, so every element of this figure sat below the floor, the
# largest and boldest included. The sizes below are the originals times 1.44, which puts
# the smallest element at 8.06 pt and the largest at 10.30 pt after the build's rescale.
# The canvas stays at 7.4 in so the author's spacing is preserved.
# If you edit this figure, check the rendered size and not the saved size. The two
# differ by the 6.2/width ratio.



ys <- c(7.30, 6.42, 5.54, 4.28, 3.52)          # five contrast rows
xL <- 2.85; xR <- 9.05                          # arrow span

boxes <- bind_rows(
  data.frame(xmin = 0.30, xmax = 2.85, ymin = 3.05, ymax = 7.85),   # IV
  data.frame(xmin = 9.05, xmax = 11.35, ymin = 5.15, ymax = 7.75),  # CTR
  data.frame(xmin = 9.05, xmax = 11.35, ymin = 3.15, ymax = 4.65),  # ToP
  data.frame(xmin = 0.30, xmax = 2.85, ymin = 0.62, ymax = 2.22),   # Study 1
  # moderator stream
  data.frame(xmin = 2.60, xmax = 4.00, ymin = 8.35, ymax = 9.15),
  data.frame(xmin = 6.95, xmax = 9.65, ymin = 8.35, ymax = 9.15)
)

xg <- 3.30; xe <- 8.30                          # moderator drop lines

p <- ggplot() +
  geom_rect(data = boxes, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "white", colour = "grey30", linewidth = 0.4) +
  # contrast arrows
  geom_segment(data = data.frame(x = xL, xend = xR, y = ys, yend = ys),
               aes(x = x, xend = xend, y = y, yend = yend),
               arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
               colour = "grey15", linewidth = 0.42) +
  # moderator drops, each terminating ON its path
  geom_segment(data = data.frame(
      x    = c(xg, xg, xe, xe, xe),
      xend = c(xg, xg, xe, xe, xe),
      y    = c(8.35, 7.22, 8.35, 7.22, 6.34),
      yend = c(7.36, 4.34, 7.36, 6.48, 5.60)),
    aes(x = x, xend = xend, y = y, yend = yend),
    arrow = arrow(length = unit(0.12, "cm"), type = "closed"),
    colour = "grey45", linewidth = 0.33) +
  # design provenance
  annotate("segment", x = 1.57, xend = 1.57, y = 2.28, yend = 3.05,
           arrow = arrow(length = unit(0.13, "cm"), type = "closed"),
           colour = "grey55", linewidth = 0.33, linetype = "dotted") +
  annotate("text", x = 1.80, y = 2.62, hjust = 0,
           label = "descriptive context for the framing contrasts\n(design provenance, not a modeled path)",
           size = 3.36, colour = "grey45", lineheight = 1.1) +
  # box text
  annotate("text", x = 1.57, y = 7.50, label = "Message framing\ncondition",
           size = 3.70, fontface = "bold", colour = "grey10", lineheight = 1.1) +
  annotate("text", x = 1.57, y = 5.95,
           label = "neutral informational\n(reference level)\n\nempathy\nsocial norm\nnarrative",
           size = 3.50, colour = "grey10", lineheight = 1.28) +
  annotate("text", x = 1.57, y = 3.98, label = "three creative variants\nper condition, delivered\nin two campaign rounds",
           size = 3.36, colour = "grey45", fontface = "italic", lineheight = 1.15) +
  annotate("text", x = 10.20, y = 6.45, label = "Click-through rate\n(primary outcome)",
           size = 3.60, colour = "grey10", lineheight = 1.1) +
  annotate("text", x = 10.20, y = 3.90, label = "Time on landing page\n(secondary outcome)",
           size = 3.60, colour = "grey10", lineheight = 1.1) +
  annotate("text", x = 1.57, y = 1.92, label = "Study 1    RQ1", size = 3.60,
           fontface = "bold", colour = "grey10") +
  annotate("text", x = 1.57, y = 1.24,
           label = "Content analysis of the\nNamibian conservation\nlandscape on Instagram",
           size = 3.40, colour = "grey10", lineheight = 1.2) +
  # moderator labels
  annotate("text", x = c(3.30, 8.30), y = 8.88,
           label = c("Gender", "Environmental concern"),
           size = 3.45, fontface = "bold", colour = "grey10") +
  annotate("text", x = c(3.30, 8.30), y = 8.55,
           label = c("H2a, H2b", "H6"),
           size = 3.40, colour = "grey35") +
  # contrast labels, above each arrow, clear of the drop lines
  annotate("text", x = rep(8.02, 5), y = ys + 0.20,
           label = c("empathy vs. neutral    H1", "social norm vs. neutral    H3",
                     "narrative vs. neutral    H4", "empathy vs. neutral",
                     "narrative vs. neutral    H5"),
           size = 3.45, colour = "grey10", hjust = 1) +
  annotate("segment", x = 0.30, xend = 11.35, y = 0.30, yend = 0.30,
           colour = "grey70", linewidth = 0.3) +
  annotate("text", x = 0.30, y = -0.62, hjust = 0,
           label = paste0("Arrows denote predicted differences in observed engagement, not causal effects.\n",
                          "Delivery followed the platform's auction, not random assignment.\n",
                          "H7   the condition rank order on click-through rate replicates across the two rounds.\n",
                          "RQ2   the four framings compared with one another."),
           size = 3.36, colour = "grey25", lineheight = 1.35) +
  coord_cartesian(xlim = c(0.15, 11.50), ylim = c(-1.25, 9.40), expand = FALSE) +
  labs(x = NULL, y = NULL) + theme_apa() +
  theme(axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.line = element_blank(),
        panel.border = element_blank())

save_apa(p, "fig_conceptual_model", width = 7.4, height = 7.1)
cat("final written\n")


# ---- Figure 5, the measurement chain ----------------------------------------
#
# The figure carries the tool and the formula for every outcome, so no separate
# instrument table is needed. APA 7.3 permits combining smaller displays of
# similar content into one, and 7.22 requires that a figure not duplicate other
# elements of the paper. The instrument band above the row names the tool, and
# the outcome boxes carry the formula, including the reach-based sensitivity
# denominator.
#
# APA 7.26 floors figure text at 8 pt. build.js rescales every embedded figure to
# a 6.2-in column, so at a 6.5-in canvas the factor is 0.954 and size 3.0
# (8.53 pt) lands at 8.14 pt. Size 3.0 is the floor here, not a preference.

SZ  <- 3.0
ARR <- arrow(length = unit(0.14, "cm"), type = "closed")

blank <- theme(plot.margin = margin(3, 4, 3, 3),
               axis.title = element_blank(), axis.text = element_blank(),
               axis.ticks = element_blank(), axis.line = element_blank(),
               panel.border = element_blank())

box <- function(x0, x1, y0, y1)
  geom_rect(data = data.frame(xmin = x0, xmax = x1, ymin = y0, ymax = y1),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "white", colour = "grey30", linewidth = 0.4)

# annotate() drops the whole row if it is handed an explicit arrow = NULL,
# silently and without an error, so the argument is added only when wanted.
seg <- function(x, xend, y, yend, arr = FALSE, col = "grey55", lw = 0.3) {
  args <- list("segment", x = x, xend = xend, y = y, yend = yend,
               colour = if (arr) "grey15" else col,
               linewidth = if (arr) 0.42 else lw)
  if (arr) args$arrow <- ARR
  do.call(annotate, args)
}

txt <- function(x, y, lab, face = "plain", col = "grey20")
  annotate("text", x = x, y = y, label = lab, size = SZ,
           fontface = face, colour = col, lineheight = 1.15)

# Five stages across a 6.5-in canvas. The three platform stages are narrower
# than the two post-click ones, because those carry the longer labels. Text
# stays at SZ, the APA 7.26 floor; the boxes move instead.
chain <- data.frame(
  xmin = c(0.30, 2.30, 4.30, 6.30, 8.75),
  xmax = c(2.00, 4.00, 6.00, 8.45, 10.80),
  lab  = c("Impressions", "3-s video\nviews", "Link clicks",
           "Landing-page\nsessions", "Exit survey"),
  sub  = c("delivered by\nthe auction", "platform\ncounter",
           "platform\ncounter", "consent-gated", "voluntary")
)

p <- ggplot() +
  # Instrument band. Three tools, one per contiguous run of stages.
  seg(0.30, 6.00, 4.62, 4.62, col = "grey60") +
  seg(6.30, 8.45, 4.62, 4.62, col = "grey60") +
  seg(8.75, 10.80, 4.62, 4.62, col = "grey60") +
  txt(3.15, 4.78, "Meta Ads Manager", col = "grey30") +
  txt(7.38, 4.78, "Google Analytics 4", col = "grey30") +
  txt(9.78, 4.78, "SoSci Survey", col = "grey30") +
  geom_rect(data = chain, aes(xmin = xmin, xmax = xmax, ymin = 3.30, ymax = 4.45),
            fill = "white", colour = "grey30", linewidth = 0.4) +
  geom_segment(data = data.frame(x = c(2.00, 4.00, 6.00, 8.45),
                                 xend = c(2.30, 4.30, 6.30, 8.75)),
               aes(x = x, xend = xend, y = 3.88, yend = 3.88),
               arrow = ARR, colour = "grey15", linewidth = 0.42) +
  geom_text(data = chain, aes(x = (xmin + xmax) / 2, y = 4.16, label = lab),
            size = SZ, fontface = "bold", colour = "grey10", lineheight = 1.15) +
  geom_text(data = chain, aes(x = (xmin + xmax) / 2, y = 3.60, label = sub),
            size = SZ, colour = "grey45", fontface = "italic", lineheight = 1.15) +
  # Primary outcome, bracketed off the impression and click stages.
  seg(1.15, 1.15, 3.30, 2.98) + seg(5.15, 5.15, 3.30, 2.98) +
  seg(1.15, 5.15, 2.98, 2.98) + seg(3.15, 3.15, 2.98, 2.76, arr = TRUE) +
  box(0.30, 6.00, 1.36, 2.76) +
  txt(3.15, 2.52, "Click-through rate", "bold", "grey10") +
  txt(3.15, 2.28, "primary outcome", "italic", "grey45") +
  txt(3.15, 2.00, "link clicks / impressions") +
  txt(3.15, 1.68, "sensitivity check:\nlink clicks / reach", col = "grey45") +
  # Secondary outcome.
  seg(7.38, 7.38, 3.30, 2.76, arr = TRUE) +
  box(6.30, 8.45, 1.36, 2.76) +
  txt(7.38, 2.52, "Time on page", "bold", "grey10") +
  txt(7.38, 2.26, "secondary outcome", "italic", "grey45") +
  txt(7.38, 1.74, "avg. session duration,\nUTM-segmented\nby condition", col = "grey20") +
  # The exit survey feeds no outcome. It supplies the moderator items.
  seg(9.78, 9.78, 3.30, 2.76, arr = TRUE) +
  box(8.75, 10.80, 1.36, 2.76) +
  txt(9.78, 2.52, "Moderator items", "bold", "grey10") +
  txt(9.78, 2.26, "no outcome", "italic", "grey45") +
  txt(9.78, 1.74, "five single items,\ncondition tag\nin the link", col = "grey20") +
  coord_cartesian(xlim = c(0.15, 10.95), ylim = c(1.24, 4.96), expand = FALSE) +
  labs(x = NULL, y = NULL) + theme_apa() + blank

save_apa(p, "fig_measurement_funnel", width = 6.5, height = 2.62)
cat("measurement chain written\n")
