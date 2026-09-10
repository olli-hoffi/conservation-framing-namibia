# =============================================================================
# src/apa_tables.R
# Turn a data frame into an APA-7 formatted Word (.docx) table via flextable.
#
# Usage:
#   my_tbl %>% apa_flextable() %>% save_flex("study2_glm_ctr", "GLM results ...")
# or in one step:
#   save_apa_table(my_tbl, "study2_glm_ctr", note = "OR = odds ratio ...")
# =============================================================================

library(flextable)
library(officer)

# theme_flex_apa(): APA-7 table styling - Times New Roman, horizontal rules only
# (top, below the header, bottom), no vertical lines, left-aligned first column.
theme_flex_apa <- function(ft) {
  big_border <- fp_border(color = "black", width = 1)
  ft %>%
    font(fontname = "Times New Roman", part = "all") %>%
    fontsize(size = 11, part = "all") %>%
    border_remove() %>%
    hline_top(border = big_border, part = "header") %>%     # rule above the header
    hline_bottom(border = big_border, part = "header") %>%  # rule below the header
    hline_bottom(border = big_border, part = "body") %>%    # rule at the foot of the table
    bold(part = "header", bold = FALSE) %>%
    align(align = "center", part = "all") %>%
    align(j = 1, align = "left", part = "all") %>%          # APA: stub (first) column left-aligned
    padding(padding = 4, part = "all") %>%
    set_table_properties(layout = "autofit")
}

# save_flex(): apply an APA title (bold "Table X", italic caption) and a note,
# then write the styled flextable to output/tables/<name>.docx.
save_flex <- function(ft, name, title = NULL, note = NULL,
                      outdir = here::here("output", "tables")) {
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
  if (!is.null(title)) ft <- add_header_lines(ft, values = title) %>%
    italic(i = 1, part = "header")
  if (!is.null(note)) ft <- add_footer_lines(ft, values = paste0("Note. ", note)) %>%
    italic(i = 1, part = "footer") %>% fontsize(size = 10, part = "footer")
  save_as_docx(ft, path = file.path(outdir, paste0(name, ".docx")))
  invisible(ft)
}

# save_apa_table(): the common one-liner - build the flextable, style it APA,
# round numeric columns, and write the .docx in a single call.
save_apa_table <- function(df, name, title = NULL, note = NULL, digits = 2) {
  df %>%
    mutate(across(where(is.numeric), ~ round(.x, digits))) %>%
    flextable() %>%
    theme_flex_apa() %>%
    save_flex(name, title = title, note = note)
}
