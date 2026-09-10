# =============================================================================
# run_all.R
# Reproduces every reported result from the raw data in one command.
#
#   Rscript run_all.R          run everything, then write the manifest
#   Rscript run_all.R --check  run nothing, only compare the outputs on disk
#                              against the manifest committed with the package
#
# Run it from the project root, or open conservation-framing-namibia.Rproj first. On a first
# run, restore the recorded package versions with renv::restore().
#
# Output: output/MANIFEST.csv    one line per produced file, with a checksum
#         output/session_info.txt the R build, platform and package versions
# =============================================================================

suppressMessages({library(here); library(tools)})

# Sorting of character vectors depends on the locale, and a different locale can
# reorder factor levels, which moves reference categories and flips the sign of a
# contrast. Fixing it to C keeps the ordering identical on every machine.
invisible(Sys.setlocale("LC_COLLATE", "C"))
# setlocale() changes only this process. Each script below runs as its own
# Rscript child, and a child inherits the environment, not the setting, so the
# variable has to be set as well or the fix never reaches them.
Sys.setenv(LC_COLLATE = "C")
# LC_ALL outranks every individual LC_* variable, so a shell that exports it would
# hand each child process the user's collation and silently undo the line above.
# Verified: with LC_ALL=en_US.UTF-8 in the environment a child sorted c("b","A","a","B")
# as aAbB instead of ABab, which is enough to reorder grouped rows and break --check
# on someone else's machine.
# It is UNSET rather than pinned to "C". Pinning it would also force LC_CTYPE to C,
# which drops the character type down to ASCII and breaks matching on the non-ASCII
# text in the corpus. That was tried and it silently emptied the sampling filters:
# the Study 1 funnel fell from 65 keyword records to 15 and from 920 posts to 2.
# Unsetting LC_ALL leaves LC_COLLATE = "C" above in force for sorting while the
# inherited LC_CTYPE keeps UTF-8 intact.
Sys.unsetenv("LC_ALL")

# Run order. Two rules fix it. The 01* scripts write data/processed/, which every
# later script reads, so they come first. Everything after follows the reading
# order of the thesis, Study 1 before Study 2.
SCRIPTS <- c(
  # Chapter 2, the schematic figures. They compute nothing and read no data.
  "analysis/00_figures_conceptual.R",                  # Figures 1 and 5

  # Chapter 3, Study 1. The order follows the sections of the chapter.
  "analysis/01_study1/01_sampling_frame.R",            # Figure 2
  "analysis/01_study1/02_reliability.R",               # Table 1, both standards
  "analysis/01_study1/03_reliability_stability.R",     # Table B2, image effect
  "analysis/01_study1/04_corpus_frame_prevalence.R",   # Table 3
  "analysis/01_study1/05_org_styles_genres.R",         # Figure 3; Table B3
  "analysis/01_study1/06_message_construction.R",      # Table 4
  "analysis/01_study1/07_attention_species_places.R",  # Figure 4; Table B4
  "analysis/01_study1/08_reception_engagement.R",      # Tables 3, 5, 6
  "analysis/01_study1/09_reception_org_model.R",       # Table 5, organization-adjusted
  "analysis/01_study1/10_reception_story_structure.R", # narrative-structure paragraph
  "analysis/01_study1/11_reception_misuse.R",          # Figure B1; likes-model passage
  "analysis/01_study1/12_taxonomy_subsample.R",        # Table 7; Table B1
  "analysis/01_study1/13_reception_comment_form.R",    # Table 6

  # Chapter 4, Study 2. The two 0x_process scripts write data/processed/, which
  # every later script reads, so they run first. The rest follows the chapter.
  "analysis/02_study2/01_process_pretest.R",           # pretest raw -> processed
  "analysis/02_study2/02_pretest_validation.R",        # Table 8; E5, E7; Figure E1
  "analysis/02_study2/03_process_campaign.R",          # Meta + GA4 + exit survey -> processed
  "analysis/02_study2/04_confirmatory.R",              # PREREGISTERED: Tables 10, 12
  "analysis/02_study2/05_confirmatory_h2a.R",          # PREREGISTERED: Table 13, H2a rows
  "analysis/02_study2/06_delivery_diagnostics.R",      # Figure 12
  "analysis/02_study2/07_delivery_combined.R",         # Figure 7
  # 09 runs BEFORE 08: 08_delivery_funnel.R reads study2_expl_engagement_funnel.csv,
  # which 09 writes. On a cleaned output/ the reverse order fails with "does not exist".
  "analysis/02_study2/09_engagement_models.R",         # Figure 8; Tables F3, F4
  "analysis/02_study2/08_delivery_funnel.R",           # Figure 10
  "analysis/02_study2/10_comment_reception.R",         # Figure 9
  "analysis/02_study2/11_dispute_lexicon.R",           # dispute-marker contrast
  "analysis/02_study2/12_hook_variants.R",             # Table 14
  "analysis/02_study2/13_figures_remaining.R",         # Figures 6, 11
  "analysis/02_study2/14_exploratory.R",               # Tables 11, 13, 16, F1
  "analysis/02_study2/15_comment_reliability.R",       # Table 15
  "analysis/02_study2/16_h7_chance_rate.R"             # H7 chance rate, 42 of 576
)

manifest_path <- here("output", "MANIFEST.csv")

# These Python feature outputs supply reported Study 1 counts but live beside the
# other feature tables rather than under output/. The R pipeline consumes the
# shipped feature state, so the manifest tracks these files without regenerating
# them. Their producing scripts and pinned environment are documented in README.md.
PYTHON_NUMERIC_OUTPUTS <- c(
  here("data/study1/features/llm_target_audience_counts.csv"),
  here("data/study1/features/caption_screen_summary.csv"),
  here("data/study1/features/caption_screen_matches.csv"),
  here("data/study1/retrieval_cap_audit.csv"),
  here("data/study1/retrieval_cap_audit_summary.csv")
)

# Rscript is addressed through R's own installation rather than the PATH, because on
# Windows it is usually not on the PATH at all. R.home("bin") always knows where the
# running R lives, so the child processes are the same R as the parent.
RSCRIPT <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
# Quoting style follows the platform. The sh rules that shQuote() defaults to produce
# the wrong thing on a Windows command line, and some paths here contain spaces.
QUOTE <- if (.Platform$OS.type == "windows") "cmd" else "sh"

# Checksum every produced file. The manifest is what makes a stale output
# detectable: if a data fix lands without a rerun, the checksums stop matching
# and the mismatch shows up here instead of in the next audit.
build_manifest <- function() {
  missing_python_outputs <- PYTHON_NUMERIC_OUTPUTS[!file.exists(PYTHON_NUMERIC_OUTPUTS)]
  if (length(missing_python_outputs)) {
    stop("Missing tracked Python output(s): ", paste(missing_python_outputs, collapse = ", "))
  }
  files <- c(list.files(here("output"), recursive = TRUE, full.names = TRUE),
             PYTHON_NUMERIC_OUTPUTS)
  files <- files[basename(files) != "MANIFEST.csv" & basename(files) != ".DS_Store"]
  data.frame(
    file  = sub(paste0("^", here(), "/"), "", files),
    bytes = file.size(files),
    md5   = unname(tools::md5sum(files)),
    stringsAsFactors = FALSE
  )[order(sub(paste0("^", here(), "/"), "", files)), ]
}

# --check compares without running. Figures and .docx embed a creation timestamp,
# so their bytes differ on every render even when nothing changed. Only .csv and
# .txt are compared, which is where the numbers live.
if ("--check" %in% commandArgs(TRUE)) {
  if (!file.exists(manifest_path)) stop("No manifest to check against. Run without --check first.")
  old <- read.csv(manifest_path, stringsAsFactors = FALSE)
  new <- build_manifest()
  cmp <- merge(old, new, by = "file", all = TRUE, suffixes = c("_manifest", "_disk"))
  num <- cmp[grepl("[.](csv|txt|md)$", cmp$file), ]
  bad <- num[is.na(num$md5_disk) | is.na(num$md5_manifest) | num$md5_disk != num$md5_manifest, ]
  cat(sprintf("Checked %d numeric outputs against the manifest.\n", nrow(num)))
  if (nrow(bad) == 0) {
    cat("All match. The committed outputs are the ones the current code produces.\n")
  } else {
    cat(sprintf("%d MISMATCH(ES). Rerun run_all.R and inspect the differences:\n", nrow(bad)))
    for (f in bad$file) cat("  ", f, "\n")
    quit(status = 1)
  }
  quit(status = 0)
}

# ---- run everything ---------------------------------------------------------
cat("Reproduction package, full run.\n")
cat(rep("-", 64), "\n", sep = "")
started <- Sys.time()
failed  <- character(0)

for (s in SCRIPTS) {
  t0 <- Sys.time()
  # Each script runs in its own R process, so nothing carries over between them.
  # A leaked object or a leftover RNG state cannot make one script depend on
  # another having run first in the same session.
  rc <- system2(RSCRIPT, shQuote(here(s), type = QUOTE), stdout = FALSE, stderr = FALSE)
  secs <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")))
  cat(sprintf("%-52s %-7s %4ds\n", s, if (rc == 0) "OK" else "FAILED", secs))
  if (rc != 0) failed <- c(failed, s)
}

cat(rep("-", 64), "\n", sep = "")
cat(sprintf("Total: %d seconds.\n", round(as.numeric(difftime(Sys.time(), started, units = "secs")))))

if (length(failed)) {
  cat("\nFAILED, so no manifest was written:\n"); for (f in failed) cat("  ", f, "\n")
  quit(status = 1)
}

# ---- record what this run produced and what produced it ---------------------
# Session info is written BEFORE the manifest, so the manifest covers it too.
# The other order leaves session_info.txt permanently mismatched against the
# manifest that was built a moment before it.
si <- file(here("output", "session_info.txt"), open = "wt")
writeLines(capture.output(sessionInfo()), si)
close(si)

mf <- build_manifest()
write.csv(mf, manifest_path, row.names = FALSE)
cat(sprintf("\nManifest: %d files, %s\n", nrow(mf), manifest_path))
cat("\nVerify a later checkout with: Rscript run_all.R --check\n")
