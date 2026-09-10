# =============================================================================
# _dependencies.R
# Packages that no script names, so renv cannot discover them by reading the
# code, but that the analysis nevertheless depends on. This file is never run.
# It exists so renv::snapshot() records them and renv::restore() installs them.
#
# GPArotation
#   psych::principal(rotate = "oblimin") loads it at call time. Without it the
#   rotation SILENTLY falls back to a different solution and returns different
#   loadings, with a warning but no error. Table 3's frame components differ
#   noticeably: Empathy loads .96 on TC1 with GPArotation and .89 without it.
#   It is therefore a hard requirement for reproducing the reported table.
# =============================================================================

library(GPArotation)
