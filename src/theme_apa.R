# =============================================================================
# src/theme_apa.R
# APA-7 ggplot2 theme + figure helpers.
# Sourced automatically at the top of every analysis script via:
#   list.files(here("src"), full.names = TRUE) %>% walk(source)
# =============================================================================

# Scripts print figure objects on a bare line so they appear in the RStudio plot
# pane. Under Rscript there is no pane, so R opens its default PDF device instead
# and leaves an Rplots.pdf in the project root. Routing the default device to a
# null device keeps the interactive behaviour and drops the stray file.
if (!interactive()) options(device = function(...) grDevices::pdf(NULL))

library(ggplot2)

# APA-7 figure theme: white panel, no gridlines, black axis lines, sans-serif.
# %+replace% swaps each named element wholesale (instead of merging), the standard
# idiom for building a custom theme on top of a base one.
# Title/subtitle/caption are blanked: under APA 7 the figure number, title and note
# are set in the document.
theme_apa <- function(base_size = 12, base_family = "sans", legend_pos = "right") {
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      panel.background  = element_rect(fill = "white", colour = NA),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),   # APA: no gridlines
      panel.grid.minor  = element_blank(),
      axis.line         = element_line(colour = "black", linewidth = 0.5),
      axis.ticks        = element_line(colour = "black", linewidth = 0.4),
      axis.text         = element_text(colour = "black", size = rel(0.85)),
      axis.title        = element_text(colour = "black", size = rel(1.0)),
      axis.title.x      = element_text(margin = margin(t = 8)),
      axis.title.y      = element_text(margin = margin(r = 8), angle = 90),
      legend.background = element_blank(),
      legend.key        = element_blank(),
      legend.position   = legend_pos,
      legend.title      = element_text(size = rel(0.9)),
      legend.text       = element_text(size = rel(0.85)),
      strip.background  = element_rect(fill = "grey92", colour = NA),
      strip.text        = element_text(colour = "black", size = rel(0.9),
                                       margin = margin(4, 4, 4, 4)),
      plot.background   = element_rect(fill = "white", colour = NA),
      plot.title        = element_blank(),
      plot.subtitle     = element_blank(),
      plot.caption      = element_blank(),
      plot.margin       = margin(10, 14, 10, 10)   # room so long axis labels are not clipped
    )
}

theme_set(theme_apa())   # make it the session default so every plot inherits it

# "Mako" palette (viridisLite::mako, begin .2 / end .85) for the four conditions:
# perceptually uniform and colourblind-safe. Runs dark navy -> blue -> teal -> mint,
# so Narrative (the highest-CTR frame) reads as the lightest, most salient colour.
condition_colours <- c(
  "Neutral"     = "#382A54",   # dark navy-purple (baseline control)
  "Empathy"     = "#38629D",   # blue
  "Social Norm" = "#35A1AB",   # teal
  "Narrative"   = "#84D9B1"    # mint green
)

# Standard APA figure dimensions (inches) and print resolution.
apa_width_full <- 6.5    # full text width
apa_width_half <- 3.25   # single column
apa_dpi        <- 300

# save_apa(p, "fig_name"): writes BOTH a 300-dpi PNG (for Word) and a vector PDF
# (for print) into output/figures/. No file extension in the name.
save_apa <- function(plot, filename,
                     width = apa_width_full, height = 4,
                     outdir = here::here("output", "figures")) {
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
  ggsave(file.path(outdir, paste0(filename, ".png")), plot,
         width = width, height = height, dpi = apa_dpi, units = "in")
  ggsave(file.path(outdir, paste0(filename, ".pdf")), plot,
         width = width, height = height, units = "in")
  invisible(NULL)
}

# ---------------------------------------------------------------------------
# Display labels for coded variables.
#
# The machine names are data column names and must stay as they are. APA 7
# section 7.12 wants variable names spelled out in sentence case wherever a
# reader sees them, which includes figure axes and legends. Apply apa_label() to
# any column before it reaches a scale or a legend.
# ---------------------------------------------------------------------------
apa_var_labels <- c(
  Empathy           = "Empathy",
  Threat            = "Threat",
  Efficacy          = "Efficacy",
  Collective_ID     = "Collective identity",
  Normative         = "Normative",
  Moral             = "Moral",
  Economic          = "Economic",
  Scientific        = "Scientific",
  Youth_Addressed   = "Youth addressed",
  Interactivity     = "Interactivity prompt",
  Youth_Style       = "Youth-oriented style",
  Protagonist       = "Protagonist",
  Onscreen_Text     = "On-image text",
  Data_Visual       = "Data visualization",
  Story_Structure   = "Story structure",
  Primary_CTA       = "Primary call to action",
  Primary_Topic     = "Primary topic",
  Emotional_Valence = "Emotional valence",
  Language          = "Language",
  Post_Format       = "Post format"
)

apa_label <- function(x) {
  x <- as.character(x)
  out <- unname(apa_var_labels[x])
  ifelse(is.na(out), x, out)
}

# ---------------------------------------------------------------------------
# Organisation display names for figures.
# Actor_Name in org_feature_matrix.csv is the scraped profile string, so it
# carries ampersands and one misspelling ("Namibia Youth Chamber", whose own
# site and reference entry read "Namibian"). The thesis prose spells all three
# out. Same problem and same remedy as apa_var_labels: fix it at the render
# boundary, not in the data file, so the feature matrix stays what the scrape
# returned. Apply apa_org() before a name reaches an axis or a legend.
# ---------------------------------------------------------------------------
apa_org_labels <- c(
  "Ministry of Environment Forestry & Tourism" = "Ministry of Environment, Forestry and Tourism",
  "Gobabeb Research & Training Centre"         = "Gobabeb Research and Training Centre",
  "Namibia Youth Chamber of Environment"       = "Namibian Youth Chamber of Environment"
)

apa_org <- function(x) {
  x <- as.character(x)
  out <- unname(apa_org_labels[x])
  ifelse(is.na(out), x, out)
}
