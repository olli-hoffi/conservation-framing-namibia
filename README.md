# Reproduction Package

Analysis code and data for the MSc thesis *Message Framing on Instagram: A Content
Analysis and Naturalistic Comparison of Conservation Communication in Namibia*
(Oliver Hoffmann, Goethe University Frankfurt).

Every number, table and figure the thesis reports is regenerated from the raw data by
the scripts in this repository. The package follows the research-compendium
convention: raw data is read-only, everything derived is produced by code, and the
computational environment ships with the code.

## How to run

Download the repository, as a ZIP from the green *Code* button or with
`git clone`, and unpack it. It is about 380 MB, most of that the twelve stimulus
videos. Then open `conservation-framing-namibia.Rproj`
in RStudio, which anchors every path and activates the recorded environment.

```r
renv::restore()            # installs the exact package versions this analysis used
source("run_all.R")        # or, from a terminal: Rscript run_all.R
```

`renv::restore()` is the only step that needs the internet, and it is quite slow,
since it fetches 203 packages at the versions recorded in `renv.lock`. Everything
after that is offline. If R does not know `renv` yet, opening the project installs it,
because `renv/activate.R` carries its own bootstrap.

`run_all.R` runs the 30 scripts in dependency order, each in its own R process. A full
run took 22 minutes on the documented machine, most of it in the story-structure step.
It then writes `output/MANIFEST.csv`, which records the size
and MD5 checksum of every tracked output, and `output/session_info.txt`. The manifest
also covers five Python-generated files that supply the reported audience-label,
caption-screen, and source-retrieval counts.

```
Rscript run_all.R --check
```

compares the files on disk against that manifest without computing anything. Use it to
confirm that the outputs shipped here are the ones the current code produces. Figures
and `.docx` files embed a creation timestamp and can never be byte-identical, so the
check covers `.csv` and `.txt`, which is where the numbers live. When regenerating the
tracked Python files, run `b2_taxonomy_features.py` and `c3_caption_features.py` before
the full R run. The retrieval diagnostics are emitted by `convert_apify_to_raw.py`,
which requires the original Apify JSON. That source file is excluded from this download
for the data-protection reason below.

## Platform

Any operating system that runs R 4.4 or later. Nothing here is specific to macOS,
Windows or Linux.

Paths are built with `here()` and `file.path()` throughout, never by pasting
separators, so a Windows backslash and a Unix slash both resolve. `run_all.R`
addresses Rscript through `R.home("bin")` instead of the PATH, because on Windows
it is usually not on the PATH, and it quotes arguments in the style the platform
expects. It also fixes `LC_COLLATE` to `C`, so string sorting cannot reorder factor
levels between machines with different locales. The data files are UTF-8. Some carry
Windows line endings and a byte-order mark, because the survey and advertising
platforms exported them that way and the files are shipped as they were received.
`readr` and `pandas` absorb both, so neither needs handling.

No working directory has to be set. Opening the project file anchors it, and every
script resolves its own paths from there.

One requirement is not automatic. `renv::restore()` builds some packages from
source on Linux, which needs the usual system development tools. On macOS and
Windows it installs prebuilt binaries and needs nothing extra.

There is one version of this package and it is the same on every platform. Splitting
it per operating system would create copies that drift apart, and a reader would not
know which one produced the reported numbers.

**Tested on** macOS 26.3 with R 4.4.0, where the full pipeline runs green and every
numeric output matches the manifest. Windows and Linux are not tested.
If something fails on your system, the contact address in the
thesis appendix reaches the author.

## What the download does not contain

The raw Apify post export and the post images are not shipped, for the reasons
given in the next section. What they produced is included.

The twelve advertisement videos ship with the repository, 365 MB in total, in
`materials/stimuli/` together with their specification and the transcripts.

Every analysis script runs from the package alone.
`data/study1/features/comments_labeled.csv` carries features but no text column, so
`11_dispute_lexicon.R` joins it on `comment_id` to `data/study1/comments_text.csv`,
which holds the text for the same 2,172 in-corpus comments with pseudonymized
handles. Pseudonymization rewrites an @-handle in 63 of those comments and none of
the 63 matches a dispute pattern, so it reaches neither the nine flagged comments nor
the cross-corpus dispute contrast of 21.1% of advertisement comments against 0.41% of
organic comments.

Everything else is included, the analysis code, the remaining data it reads, and
every table and figure it writes.

## What is and is not reproducible

Read this before concluding something is broken.

| Part | Reproducible offline | Why not |
|---|---|---|
| All R analysis (`analysis/`) | **Yes**, end to end | |
| Study 1 feature generation (`python/study1/`) | Eight of eighteen scripts | `a5`, `a6`, `b2`, `c1`, `c2`, `c3`, `c5` and `study1_nlp_pipeline` rerun byte for byte. The other ten fail for one of the reasons below. `python/README.md` names the reason per script. |
| Post collection (`convert_apify_to_raw`, `c6`, `c7`, `00_build_analysis_base`) | **No** | Need the raw Apify JSON, which is not shipped because it carries the account name and profile picture of every commenter. What it produced is shipped, including `data/study1/captions.csv`, the caption text of all 920 posts. That file carries no commenter data. Its rows are organizational posts, so the caption analyses run offline. |
| Image features (`a2`, `a3`, `c4`) | **No** | The package ships no post images. They are the organizations' own photographs, one cover image per coded post, 917 files and 291 MB. The feature CSVs they produced are included. |
| LLM passes (`code_all_posts_multimodal`, `b1`) | **No** | Drive a language-model CLI. Their output is not deterministic, so the committed CSVs are the canonical record. |
| Caption topic model (`study1_nlp_pipeline`) | **Yes**, byte for byte | Needs the embedding and topic-model packages of `requirements.txt`, a large install because sentence-transformers pulls torch. The shipped embedding cache means a rerun reloads the saved vectors instead of re-encoding, so the 920 cluster ids come out identical. |
| Comment topic model (`c6`) | **No** | Needs the raw Apify comment export, which is not shipped. |

Nothing in the reported results depends on rerunning the steps above. They produced
inputs that are committed here.

## Folder layout

```
Reproduction/
├── run_all.R               reproduce everything, then verify with --check
├── renv.lock               the exact R and package versions used
├── _dependencies.R         packages no script names but the analysis needs
├── conservation-framing-namibia.Rproj      open this in RStudio
├── analysis/
│   ├── 00_figures_conceptual.R   the two schematic figures
│   ├── 01_study1/                Chapter 3, in the order of the chapter
│   └── 02_study2/                Chapter 4, in the order of the chapter
├── src/                    shared helpers, sourced by every script
│   ├── theme_apa.R           APA-7 ggplot theme, condition colours, save_apa()
│   ├── apa_tables.R          flextable -> APA-7 .docx
│   ├── apa_format.R          fmt_p / fmt_r / fmt_ci / fmt_d for in-text reporting
│   └── quasi_diagnostics.R   dispersion checks for the quasibinomial models
├── data/
│   ├── raw/                untouched exports (SoSci, Apify, Meta Ads, GA4)
│   ├── processed/          written by the 0x_process scripts, never edited by hand
│   └── study1/             Study 1 master table + Python-generated feature CSVs
├── output/
│   ├── tables/             CSV per result, plus an APA-7 .docx of each reported table
│   ├── figures/            PNG (300 dpi) + PDF (vector)
│   ├── MANIFEST.csv        checksum of every produced file
│   └── session_info.txt    R build, platform, package versions
├── materials/              study materials, the largest part of the download
│   ├── stimuli/            the twelve advertisement videos, specs and transcripts
│   ├── pretest/            questionnaire instrument and screenshots
│   ├── exit_survey/        questionnaire screenshots
│   └── landing_page/       the page the advertisements pointed to
└── python/                 feature generation, where the corpus becomes measurable
    ├── study1/           all of it belongs to Study 1; Study 2 needs none
    │   ├── 1_collect/      Apify export -> the RAW post CSV
    │   ├── 2_text_model/   frame scores and the topic model
    │   ├── 3_llm_coding/   the multimodal LLM coding pass
    │   └── 4_features/     00, a*, b*, c*: the 54 feature tables
    ├── _paths.py         where every input lives
    └── requirements.txt  pinned dependencies (see python/README.md)
```

## Script to thesis object

**Chapter 3, Study 1** (`analysis/01_study1/`)

| Script | Produces | Thesis object |
|---|---|---|
| `01_sampling_frame.R` | organization identification and screening funnel | Figure 2 |
| `02_reliability.R` | machine codes against both human standards, the anchored review and the independent blind coding | Table 1 |
| `03_reliability_stability.R` | run-to-run agreement, the display image's contribution | Table B2 |
| `04_corpus_frame_prevalence.R` | frame prevalence with Wilson intervals | Table 3 |
| `05_org_styles_genres.R` | communication-style clusters, actor profiles, retrieved-span cadence, follower range, and recurring named formats | Figure 3; Table B3; organizational-results passage |
| `06_message_construction.R` | voice, emotion, moral foundations, frame co-occurrence | Table 4 |
| `07_attention_species_places.R` | species attention against IUCN threat status, places, network | Figure 4; Table B4 |
| `08_reception_engagement.R` | discussion ratio per frame, negative-binomial comment model, commenter reach | Tables 3, 5, 6 |
| `09_reception_org_model.R` | the comment model with a random intercept plus convergence and influence checks; internal simulated-fit checks | Table 5, organization-adjusted columns; internal model diagnostics |
| `10_reception_story_structure.R` | story structure against comments and the likes rate | narrative-structure paragraph |
| `11_reception_misuse.R` | framing and call-to-action type against engagement, likes-rate mixed model | Table B5; Figure B1; CTA likes-rate passage |
| `12_taxonomy_subsample.R` | the analyses on the *n* = 150 subsample, comment stance and affect | Table 7; Table B1 |
| `13_reception_comment_form.R` | length, form and repeat rate of the comments, plus comment topics by primary CTA | Table 6; comment-topic passage |

**Chapter 4, Study 2** (`analysis/02_study2/`). The two scripts marked
**preregistered** carry every confirmatory test. Everything else is exploratory.

| Script | Produces | Thesis object |
|---|---|---|
| `01_process_pretest.R` | processed pretest CSVs | — |
| `02_pretest_validation.R` | manipulation-check descriptives, reliability, tests | Table 8; Tables E5, E7; Figure E1 |
| `03_process_campaign.R` | processed Meta Ads, GA4 and exit-survey CSVs | — |
| `04_confirmatory.R` **preregistered** | delivery diagnostics, the primary CTR family (H1, H3, H4), H5, H7, session capture | Tables 10, 12; the H5, H7 and session-capture passages |
| `05_confirmatory_h2a.R` **preregistered** | the registered H2a contrast and its sensitivity check | Table 13, H2a rows |
| `06_delivery_diagnostics.R` | richer Meta delivery data | Figure 10 |
| `07_delivery_combined.R` | combined delivery and engagement metrics | Figure 7 |
| `08_delivery_funnel.R` | realized delivery-to-engagement funnel | Figure 9 |
| `09_engagement_models.R` | engagement GLMs, condition-level funnel, hook-rate plot | Figure F1; Tables F3, F4 |
| `10_comment_reception.R` | advertisement-comment stance analysis and complete coded comment table | Figure 8; comment-reception passage |
| `11_dispute_lexicon.R` | fixed lexical dispute-marker contrast across both corpora | dispute-lexicon passage |
| `12_hook_variants.R` | hook-clip decomposition, post-hook conversion | Table 14 |
| `13_figures_remaining.R` | variant spread, demographic heatmap | Figures 6, F2 |
| `14_exploratory.R` | pooled comparisons, moderators, demographic breakdowns, hook-CTR association | Tables 11, 16, F1; descriptive H6-related coefficients in the Results |
| `15_comment_reliability.R` | agreement across the three readings of the advertisement comments | Table 15 |
| `16_h7_chance_rate.R` | chance rate of the combined H7 criterion, full enumeration | the "42 of 576" statistic in the H7 passage |

Several body objects draw on the Python feature tables as well as an R script. Table 4
rests on the caption features from `python/study1/4_features/c3_caption_features.py`,
summarized by `06_message_construction.R`, and Table B3 combines
`05_org_styles_genres.R` with the temporal features from `c2_temporal_features.py`.
The intended-audience counts come from `b2_taxonomy_features.py`. The behavioral-
request and citizen-science lower bounds come from `c3_caption_features.py`, using
the versioned rules in `caption_screen_terms.csv`. Their summary and match-level
CSVs are tracked by `run_all.R` even though the R pipeline does not regenerate them.
The per-profile ceiling and retained-series dates come from the retrieval diagnostics
in `python/study1/1_collect/convert_apify_to_raw.py`. Its two aggregate CSVs are shipped
and tracked, while the source JSON remains excluded.
Table 9 is a documentation table with no computed source, as are the
codebook tables in Appendix A and Tables E1, E2, E4 and E6. Table E3 reports the
realized duration of each advertisement, measured from the shipped files with
`ffprobe -show_entries format=duration materials/stimuli/*.mp4`, which is left out of
the pipeline because it needs ffmpeg and renv cannot pin a system binary. Its Section C
column is the `c` section boundary in `materials/stimuli/transcripts/*.json`. Nine of the
eleven columns of Table F2 come from `03_process_campaign.R` and are held in
`data/processed/meta_ads_clean.csv`. Its average watch time is read from
`MetaAds_richmetrics_*.csv` in `06_delivery_diagnostics.R`, and its cost per click is
spend divided by link clicks. This column is regenerated
against the object headings of the thesis whenever tables or figures are renumbered;
the script header comments carry the same numbers.

## Thesis object to file

Every reported table has a CSV under `output/tables/`, apart from Table F2, which the
thesis assembles from the processed Meta data named above. Those set as APA objects carry
a formatted `.docx` beside the CSV, 36 of them. The remaining CSVs are intermediate
results that no table in the thesis prints. To go from a number in the thesis back to its source, find the object in
the table above, open the script named there, and read the result comment that follows
the calculation. Each states the computed value and what it means.

`output/tables/study2_comment_coded_review.csv` is the complete reader-facing
advertisement-comment table. It pairs all 71 records and every available comment
text with their condition and resolved codes. It contains no usernames or permalinks.

## Reading the data

`data/DATA_DICTIONARY.md` documents every column of every dataset the analyses
read: its type, its range or allowed values, how many cells are missing, and what
it means. Every type, range and missing count was read off the files themselves.

`data/study1/captions.csv` holds the caption text of all 920 posts, keyed by
`Post_ID`. The caption word shares reported in Chapter 3 run on the 910 posts that
are both coded and carry caption text. Before this file shipped, that count fell
back on the 184-post blind reliability subsample and did not match the chapter.

Most column names follow one of three prefixes, and knowing them makes the wide
tables readable at a glance. `f_` is a binary message-frame code, `m_` a
categorical one, and `MANUAL_` a human code. The dictionary opens with
the full list.

## Conventions

- **Style.** tidyverse throughout, magrittr `%>%`. Objects carry semantic prefixes
  (`data_`, `id_`, `scale_`). Non-obvious operators are explained inline.
- **Comments.** After every calculation a `#` comment states the result and what it
  means. Before a calculation, one or two lines say why that procedure is the right
  one, with a source where one exists.
- **Tables.** Every result is written twice, as a CSV for reuse and an APA-7 `.docx`
  via `save_apa_table()`.
- **Figures.** All use `theme_apa()`. The four framing conditions use the
  colourblind-safe Mako palette, other groupings use viridis or greyscale. Saved at
  300 dpi (PNG) and as vector (PDF) with `save_apa()`. A figure that no part of the
  thesis embeds is not produced.
- **Randomness.** Every stochastic step sets a seed. `run_all.R` fixes `LC_COLLATE`
  to `C`, because string sorting is locale-dependent and could otherwise reorder
  factor levels and move reference categories.
- **Confirmatory and exploratory.** Study 2's preregistered tests (AsPredicted,
  10 June 2026) live in the two scripts marked above and nowhere else. Study 1 is
  exploratory throughout and was not preregistered.

## Data protection

Comments were collected from public Instagram posts. Everyone who wrote one appears
under a stable pseudonym (`C0001` to `C1271`) that carries across every file, and the
mapping to the original account names is not part of this repository. The
advertisement comments ship without the `username` and `permalink` columns for the
same reason. Exit-survey responses contain no identifying information, since the
survey collected none.

Analysis code is under the MIT licence in `LICENSE`. The data files are shared for
verification and reuse with attribution to the thesis.

## Contact

Questions about the code or the data, and anything this README does not cover, go to
the address given in Appendix C of the thesis.
