# =============================================================================
# 02_study2/11_dispute_lexicon.R
#
# WHERE THIS BELONGS
#   In the thesis: Chapter 4, Results: Exploratory Analyses
#   Produces: the dispute-lexicon passage, no table
#   Status: Exploratory. Study 2 has a preregistration, but this script is not
#   part of it. Only 04_confirmatory.R and 05_confirmatory_h2a.R are, and any
#   result here is a finding to be retested rather than a test.
#
# Study 2 - EXPLORATORY, NON-preregistered.
#
# PURPOSE
# Chapter 4 ("Exploratory Analyses") claims that a single fixed lexical rule for
# explicit dispute markers, applied unchanged to both comment corpora, separates
# the paid advertisement audience from the organic Study 1 audience. This script
# specifies that rule and computes the contrast.
#
# WHAT THE RULE IS FOR
# The two corpora were coded by different people, under different schemes, toward
# different objects, on different denominators (see 04f header and
# 07_two_publics_contrast.py "confound audit"). A dispute gap measured on those
# codes is therefore confounded with the coding itself. A single fixed lexicon,
# applied to raw text on both sides with no per-corpus tuning, removes coder,
# scheme and denominator as explanations. It removes nothing else: sender
# credibility, message topic, delivery mode and audience composition all remain.
#
# ------------------------------ PRE-SPECIFICATION ----------------------------
# The lexicon below was fixed BEFORE it was applied to either corpus, from the
# definition given in INCLUSION/EXCLUSION, and was NOT revised afterwards. It was
# not tuned to reproduce any previously reported count. Whatever it returns is
# reported as it falls.
#
# INCLUSION CRITERION
#   A token or phrase is an explicit dispute marker only if its CORE LEXICAL
#   MEANING is (a) the assertion that a proposition is untrue, or (b) the overt
#   performance of denial or contradiction. The marker must carry that meaning by
#   itself, not by context, tone or implicature.
#
# EXCLUSION CRITERION
#   Excluded, with reasons, because each fails the criterion above:
#     - bare negation ("no", "not", "never", "nope"): negates a clause, does not
#       predicate falsity of a claim.
#     - insult and evaluation ("stupid", "idiot", "disgusting", "shame"):
#       hostility, not a truth claim. The ad-comment codebook counts hostile
#       dismissal as disputing; this rule deliberately does not.
#     - hedged scepticism ("doubt", "really?", "sure", "hmm"): withholds assent,
#       does not assert falsity.
#     - vulgar dismissal ("crap", "shit", Afrikaans "kak"): rejects without
#       claiming untruth. Contrast "bullshit", whose core sense IS insincere
#       falsehood, and which is therefore included.
#     - "cap" / "capping" (slang for lying): collides with the hat and ceiling
#       senses; too ambiguous for a fixed rule.
#     - discourse markers ("actually", "in fact", "but"): mark contrast, not
#       falsity.
#     - bare Oshiwambo "oshili" (= truth): would flag affirmations of truth as
#       readily as denials. Only the NEGATED forms are included.
#
# DIRECTIONAL CONSEQUENCE
#   Because only explicit markers count, the rule UNDERCOUNTS dispute in both
#   corpora. It bounds dispute from below on both sides; it is not an estimate of
#   how much dispute each audience expressed.
#
# KNOWN FALSE-POSITIVE RISK, ACCEPTED, NOT PATCHED
#   "lie / lies / lying" also mean recline; "wrong" also occurs in "what is wrong
#   with people" and "wrong place". No exception is carved for these, because
#   carving exceptions after seeing the data is exactly the tuning this rule
#   exists to avoid. Every flagged comment is exported for inspection.
#
# LANGUAGE SCOPE
#   Primary rule: English, Afrikaans, Oshiwambo. Those are the languages the ad
#   comments were coded into (57 english, 3 afrikaans, 3 oshiwambo_local,
#   1 mixed, 7 na) and the languages of the Namibian target audience.
#   The organic corpus additionally carries a langdetect scatter (pt, es, fr, de,
#   nl, da, ro, tl, so, tr, pl, cy), most of it noise on very short strings. A
#   primary rule scoped to three languages can therefore undercount the ORGANIC
#   side, which would inflate the gap. To bound that, a SENSITIVITY EXTENSION
#   (German/Dutch/Portuguese/Spanish/French/Italian falsity roots) is declared
#   here, up front, and reported as a second arm. It can only add hits to the
#   organic side, so declaring it is a conservatism check, not tuning.
#
# OSHIWAMBO CAVEAT
#   Coverage is limited to the distinctive "fundja" lie-root plus a small set of
#   negated-"oshili" phrases. This is the lowest-confidence component of the
#   lexicon and certainly undercounts.
# -----------------------------------------------------------------------------
#
# Inputs:
#   data/raw/MetaAds/MetaAds_comments_2026-07-09.csv   71 recovered ad comments
#   data/raw/MetaAds/MetaAds_comments_coding.csv       author stance codes (idx)
#   data/study1/features/comments_labeled.csv          2,172 in-corpus organic
#                                                      comments - DE-IDENTIFIED,
#                                                      used only to verify the
#                                                      denominator and the ids
#   data/study1/comments_text.csv                     the SAME 2,172 comments
#                                                      WITH text, de-identified
#                                                      with pseudonymized handles.
# Outputs:
#   output/tables/study2_expl_dispute_lexicon_terms.csv    the rule, one row per
#                                                          marker family
#   output/tables/study2_expl_dispute_lexicon_contrast.csv the headline contrast
#   output/tables/study2_expl_dispute_lexicon_flagged.csv  every flagged comment
#
# out: output/tables/study2_expl_dispute_lexicon_terms.csv    (the rule itself)
#      output/tables/study2_expl_dispute_lexicon_flagged.csv  (flagged comments)
#      output/tables/study2_expl_dispute_lexicon_contrast.csv (cross-corpus contrast)
#      output/tables/study2_expl_dispute_cluster_ci.csv       (cluster bootstrap CI)
# =============================================================================

# pacman::p_load() installs a package if missing, then loads it, in one call.
pacman::p_load(here, tidyverse)


# ---- 1. The lexicon ---------------------------------------------------------
# One row per marker family. `pattern` is an ICU regex applied case-insensitively
# to normalised text (see section 2). `why` records why the family satisfies the
# inclusion criterion. Fixed before application; not revised after.

lexicon <- tribble(
  ~lang,        ~family,          ~pattern, ~why,

  # --- English: falsity predicates -------------------------------------------
  "en", "lie",
  "\\b(?:lie|lies|lied|liar|liars|lying)\\b",
  "Canonical assertion that a statement is untrue and knowingly so.",

  "en", "false",
  "\\b(?:false|falsely|falsehood|falsehoods)\\b",
  "Direct predicate of truth value.",

  "en", "untrue",
  "\\b(?:untrue|untruth|untruths)\\b",
  "Direct predicate of truth value, negative pole.",

  "en", "not_true",
  "\\b(?:not|isn'?t|ain'?t|aren'?t|wasn'?t|weren'?t|hardly)\\s+(?:even |entirely |really |quite |completely |exactly |fully )?true\\b",
  "Explicit negation of the truth predicate; the most common way to say it.",

  "en", "not_the_truth",
  "\\b(?:not\\s+(?:the\\s+)?truth|far\\s+from\\s+the\\s+truth)\\b",
  "Nominal variant of the same explicit negation.",

  "en", "fake",
  "\\b(?:fake|fakes|faked|faking|fakery)\\b",
  "Asserts the object is fabricated rather than genuine. Covers 'fake news'.",

  "en", "hoax",
  "\\b(?:hoax|hoaxes)\\b",
  "Asserts the claim is a deliberate fabrication.",

  "en", "propaganda",
  "\\bpropaganda\\b",
  "Asserts the message is deceptive persuasion rather than information.",

  "en", "misinformation",
  "\\b(?:misinformation|disinformation|misinformed)\\b",
  "Names the content as false information.",

  "en", "misleading",
  "\\b(?:mislead|misleads|misleading|misled)\\b",
  "Asserts the message induces a false belief.",

  "en", "deceive",
  "\\b(?:deceive|deceives|deceived|deceiving|deceit|deceitful|deception|deceptive)\\b",
  "Asserts intent to induce a false belief.",

  "en", "fraud_scam",
  "\\b(?:fraud|frauds|fraudulent|scam|scams|scammer|scammers|scamming)\\b",
  "Asserts the sender is not what it represents itself to be.",

  # These three were one family, "nonsense", when the rule was fixed. They were
  # split into three AFTER the run, for reporting only: the union of the three
  # patterns is character-for-character the original alternation, so the set of
  # flagged comments is unchanged. The split exists because "rubbish" turned out
  # to fire on litter in a marine-debris corpus, and a single label hid that.
  "en", "nonsense",
  "\\bnonsense\\b",
  "Explicit rejection of the truth-content of what was said.",

  "en", "rubbish",
  "\\brubbish\\b",
  "Explicit rejection of the truth-content. Polysemous with 'litter' (see header).",

  "en", "bullshit",
  "\\b(?:bullshit|bullshits|bullsh\\*?t|bs)\\b",
  "Core sense is insincere falsehood, so it asserts untruth, unlike 'crap'.",

  "en", "wrong",
  "\\b(?:wrong|incorrect|inaccurate|inaccurately)\\b",
  "Asserts error in the claim. High false-positive risk, accepted (see header).",

  "en", "disagree",
  "\\b(?:disagree|disagrees|disagreed|disagreeing)\\b",
  "Overt performative of contradiction.",

  # --- Afrikaans -------------------------------------------------------------
  "af", "af_leuen",
  "\\b(?:leuen|leuens|leuenaar|leuenaars|leuenagtig)\\b",
  "lie / lies / liar. Direct equivalent of the English 'lie' family.",

  "af", "af_lieg",
  "\\b(?:lieg|liege|liegs|gelieg|liegery|jok|jokke|gejok|jokkery)\\b",
  "to lie / to fib. Verb forms of the same falsity assertion.",

  "af", "af_vals",
  "\\b(?:vals|valse|valsheid)\\b",
  "false / falseness.",

  "af", "af_onwaar",
  "\\b(?:onwaar|onwaarheid|onwaarhede)\\b",
  "untrue / untruth.",

  "af", "af_verkeerd",
  "\\b(?:verkeerd|verkeerde)\\b",
  "wrong / mistaken.",

  "af", "af_nie_waar",
  "\\bnie\\s+waar(?:\\s+nie)?\\b",
  "'not true', the standard Afrikaans negated-truth construction.",

  "af", "af_twak",
  "\\b(?:twak|twakpraatjies)\\b",
  "nonsense / rubbish. Idiomatic South African and Namibian rejection of a claim.",

  "af", "af_bedrog",
  "\\b(?:bedrog|bedrieg|bedrieglik|misleidend|misleidende)\\b",
  "fraud / deceive / misleading.",

  # --- Oshiwambo (Oshindonga / Oshikwanyama) ---------------------------------
  "osh", "osh_fundja",
  "fundja",
  paste("Distinctive lie-root: ofundja, iifundja, oifundja, omafundja,",
        "okufundja ('to lie'). Matched as a substring because the root is",
        "specific enough and the prefix morphology is variable."),

  "osh", "osh_not_truth",
  "\\b(?:ka\\s?shi\\s+shi\\s+oshili|kashi\\s+oshili|ka\\s+shi\\s+oshili|hasho\\s+oshili|ito\\s+popi\\s+oshili|inashi\\s+kala\\s+oshili)\\b",
  "Negated forms of oshili ('truth'). Bare oshili is excluded: it means truth."
)

# Sensitivity extension. Declared here, before application, for the reason given
# in the header. Reported as a SECOND arm, never folded into the primary rule.
lexicon_ext <- tribble(
  ~lang,     ~family,   ~pattern, ~why,
  "de_nl", "de_nl_luege",
  "\\b(?:l[u\u00fc]ge|l[u\u00fc]gen|l[u\u00fc]gner|luege|luegen|gelogen|falsch|unwahr|quatsch|bl[o\u00f6]dsinn|leugen|leugens|leugenaar)\\b",
  "German/Dutch falsity roots: Luege, falsch, unwahr, Quatsch, leugen.",

  "pt_es", "pt_es_mentira",
  "\\b(?:mentira|mentiras|mentiroso|mentirosa|mentindo|mentir|miente|mienten|mentes|falso|falsa|falsos|falsas|enga[n\u00f1]o|enga[n\u00f1]ar|enganar)\\b",
  "Portuguese/Spanish falsity roots: mentira, falso, engano.",

  "fr_it", "fr_it_mensonge",
  "\\b(?:mensonge|mensonges|menteur|menteuse|mensonger|faux|fausse|bugia|bugie|bugiardo|bugiarda)\\b",
  "French/Italian falsity roots: mensonge, faux, bugia."
)


# ---- 2. Normalisation and matching ------------------------------------------
# Applied identically to both corpora. Lowercase, fold typographic apostrophes to
# ASCII so "isn't" and "isn't" match the same pattern, collapse whitespace.
# Missing text becomes the empty string and can never match. No stemming, no
# lemmatisation, no spell correction.

normalise <- function(x) {
  x %>%
    replace_na("") %>%
    str_to_lower() %>%
    str_replace_all("[\u2019\u2018\u00b4\u0060]", "'") %>%
    str_squish()
}

# Returns, for each text, the comma-separated marker families that fired ("" if
# none). Vectorised over `texts`; loops over the (short) lexicon.
which_markers <- function(texts, lex) {
  hits <- map(seq_len(nrow(lex)), function(i) {
    ifelse(str_detect(texts, regex(lex$pattern[i], ignore_case = TRUE)),
           lex$family[i], NA_character_)
  })
  pmap_chr(hits, ~ paste(na.omit(c(...)), collapse = ","))
}


# ---- 3. Corpus A: the 71 advertisement comments -----------------------------

ads <- here("data/raw/MetaAds/MetaAds_comments_2026-07-09.csv") %>%
  read_csv(show_col_types = FALSE) %>%
  mutate(idx = row_number())

ad_codes <- here("data/raw/MetaAds/MetaAds_comments_coding.csv") %>%
  read_csv(show_col_types = FALSE)

ads <- ads %>%
  left_join(ad_codes, by = "idx") %>%
  mutate(txt = normalise(comment_text),
         markers     = which_markers(txt, lexicon),
         markers_ext = which_markers(txt, bind_rows(lexicon, lexicon_ext)),
         flagged     = markers     != "",
         flagged_ext = markers_ext != "")

# Denominator must be the 71 the chapter reports, and every comment must carry a
# stance code. Fail loudly rather than silently reporting a different base.
stopifnot(nrow(ads) == 71, !anyNA(ads$stance))


# ---- 4. Corpus B: the 2,172 in-corpus organic comments ----------------------
# The text-bearing organic corpus ships with the package as
# data/study1/comments_text.csv. It carries the same 2,172 in-corpus comments as
# comments_labeled.csv, de-identified in the same way, with @-mentions replaced by
# stable pseudonyms. comments_labeled.csv carries no text column and
# comments_text.csv does, so the packaged file is the one read here and the
# contrast reproduces from the package alone.
#
# The two sources were compared before the switch. The 2,172 texts differ in 63
# comments, in every case only because an @-handle is pseudonymized in the packaged
# file. None of those 63 matches any dispute pattern, and both sources yield the
# same 9 flagged comments, so the switch leaves every reported number unchanged.
#
# comments_text.csv carries the in-corpus set already, so no in_corpus filter is
# applied here. Its comment_id is written as an exact integer, unlike the float
# round-trip in corpus_flat3, which is why the identity check below compares the
# packaged file against itself through as.numeric().

ORGANIC_TEXT <- here("data/study1/comments_text.csv")
PACKAGED_FEAT <- here("data/study1/features/comments_labeled.csv")

if (!file.exists(ORGANIC_TEXT)) {
  stop(paste0(
    "\n11_dispute_lexicon.R cannot run without the text-bearing organic corpus.\n",
    "  expected at : ", ORGANIC_TEXT, "\n",
    "  affected    : the cross-corpus dispute contrast and every output this\n",
    "                script writes. The rest of run_all.R completes normally.\n"),
    call. = FALSE)
}

# comments_labeled.csv carries the features (src_org, in_corpus, topic and the
# rest), comments_text.csv carries the text. Both write comment_id as an exact
# integer, so the join is lossless; the float round-trip that damaged ids in the
# former vault source does not arise here.
# comment_id is a 17-digit platform id. Left to type inference, readr parses it as
# a double, which silently rounds it and breaks the join, so it is read as text on
# both sides.
packaged <- read_csv(PACKAGED_FEAT, show_col_types = FALSE,
                     col_types = cols(comment_id = col_character()))
org_text <- read_csv(ORGANIC_TEXT, show_col_types = FALSE,
                     col_types = cols(comment_id = col_character())) %>%
  dplyr::select(comment_id, text)

# 32 of the 2,172 comments are emoji-only and carry no text in either source.
# readr reads those as NA, so they are coerced back to "" and simply match no
# dispute pattern, which is how the former vault source behaved as well.
org <- packaged %>%
  filter(in_corpus == 1) %>%
  left_join(org_text, by = "comment_id") %>%
  mutate(text = replace_na(text, ""))

stopifnot(
  nrow(org) == 2172,
  nrow(packaged) == 2172,
  setequal(org$comment_id, org_text$comment_id)
)

org <- org %>%
  mutate(txt = normalise(text),
         markers     = which_markers(txt, lexicon),
         markers_ext = which_markers(txt, bind_rows(lexicon, lexicon_ext)),
         flagged     = markers     != "",
         flagged_ext = markers_ext != "")


# ---- 5. Interval estimation --------------------------------------------------
# Wilson score interval, and Newcombe (1998) method 10 hybrid-score interval for
# the difference of two independent proportions. Replicated exactly from
# 07_two_publics_contrast.py so the two analyses agree by construction.

wilson <- function(k, n, z = 1.959963985) {
  if (n == 0) return(c(NA_real_, NA_real_))
  p <- k / n
  den <- 1 + z^2 / n
  ctr <- (p + z^2 / (2 * n)) / den
  hw  <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / den
  c(max(0, ctr - hw), min(1, ctr + hw))
}

newcombe <- function(k1, n1, k2, n2) {
  p1 <- k1 / n1; p2 <- k2 / n2
  w1 <- wilson(k1, n1); w2 <- wilson(k2, n2)
  lo <- (p1 - p2) - sqrt((p1 - w1[1])^2 + (w2[2] - p2)^2)
  hi <- (p1 - p2) + sqrt((w1[2] - p1)^2 + (p2 - w2[1])^2)
  c(diff = p1 - p2, lo = max(-1, lo), hi = min(1, hi))
}

# Builds one result row for a given pair of counts.
contrast_row <- function(label, k_ad, n_ad, k_org, n_org) {
  wa <- wilson(k_ad, n_ad); wo <- wilson(k_org, n_org)
  nc <- newcombe(k_ad, n_ad, k_org, n_org)
  ft <- fisher.test(matrix(c(k_ad, n_ad - k_ad, k_org, n_org - k_org),
                           nrow = 2, byrow = TRUE))
  tibble(arm = label,
         ad_k = k_ad, ad_n = n_ad, ad_pct = 100 * k_ad / n_ad,
         ad_ci_lo = 100 * wa[1], ad_ci_hi = 100 * wa[2],
         org_k = k_org, org_n = n_org, org_pct = 100 * k_org / n_org,
         org_ci_lo = 100 * wo[1], org_ci_hi = 100 * wo[2],
         diff_pp = 100 * nc[["diff"]],
         diff_ci_lo = 100 * nc[["lo"]], diff_ci_hi = 100 * nc[["hi"]],
         fisher_p = ft$p.value,
         odds_ratio = unname(ft$estimate))
}

contrast <- bind_rows(
  contrast_row("primary (en + af + osh)",
               sum(ads$flagged), nrow(ads), sum(org$flagged), nrow(org)),
  contrast_row("sensitivity (+ de/nl/pt/es/fr/it)",
               sum(ads$flagged_ext), nrow(ads),
               sum(org$flagged_ext), nrow(org))
)


# ---- 6. Cross-check against the independent stance codes --------------------
# The chapter states that every lexically flagged ad comment had independently
# been coded as disputing. The lexicon and the codes were produced by different
# procedures, so this is a real check, not a tautology.

flagged_ads <- ads %>% filter(flagged)

stance_of_flagged <- flagged_ads %>% count(stance, name = "n")

concordance <- tibble(
  n_flagged            = nrow(flagged_ads),
  n_flagged_disputing  = sum(flagged_ads$stance == "disputing"),
  pct_flagged_disputing = 100 * mean(flagged_ads$stance == "disputing"),
  n_disputing_total    = sum(ads$stance == "disputing"),
  n_disputing_flagged  = sum(ads$stance == "disputing" & ads$flagged),
  sensitivity_pct      = 100 * sum(ads$stance == "disputing" & ads$flagged) /
                               sum(ads$stance == "disputing")
)


# ---- 6b. POST-HOC inspection of the organic hits ----------------------------
# STRICTLY POST-HOC. This does NOT change the rule and does NOT replace the
# pre-specified count in section 5, which stands as the result. It exists because
# the chapter makes a claim about the COMPOSITION of the organic hits ("two of
# the four contested a third party"), and that claim can only be checked by
# reading them.
#
# Each hit is judged against the inclusion criterion already stated in the
# header: does the marker, in this comment, actually assert that a proposition is
# untrue? The verdicts below were written after reading the nine texts, and the
# reasons are recorded so they can be disputed.
#
# The two collision sources are worth naming, because neither was foreseeable
# from the lexicon alone. "rubbish" is polysemous between "nonsense" and
# "litter", and the organic corpus is dominated by a marine-debris-removal
# account (oceanconservationnamibia), so the litter sense is everywhere. "lying"
# is polysemous between the falsity verb and the recline verb. Both risks were
# declared in the header before the run; they are NOT patched out afterwards.

adjudication <- tribble(
  ~comment_id,         ~verdict,          ~target,       ~reason,
  "18083851031617744", "false_positive",  NA_character_,
  "'What is wrong with him' - 'wrong' means amiss, not in error about a claim.",
  "17923119324317254", "genuine_dispute", "own_sender",
  "Counter-claim against the account's own premise about seals and fish stocks.",
  "17880396195553474", "false_positive",  NA_character_,
  "'bags of rubbish' - litter sense. The comment is praise, not dispute.",
  "18126514357589692", "false_positive",  NA_character_,
  "'collect the collected rubbish' - litter sense.",
  "18192660391317856", "genuine_dispute", "own_sender",
  "Standalone 'Bs' - bullshit, an explicit rejection of the post's content.",
  "18079302653403360", "genuine_dispute", "own_sender",
  "Standalone 'Bs' - as above.",
  "18067639367320272", "genuine_dispute", "third_party",
  "Calls Oxytane International's claims FAKE. Disputes a third party, not the ministry that posted.",
  "17882602857523896", "false_positive",  NA_character_,
  "'lying awake' - recline sense. The declared false-positive risk, realised.",
  "18086290097368296", "false_positive",  NA_character_,
  "'rubbish clearing crew' - litter sense."
)

org_flagged <- org %>%
  filter(flagged) %>%
  mutate(comment_id_chr = format(round(as.numeric(comment_id)), scientific = FALSE) %>% str_trim()) %>%
  left_join(adjudication, by = c("comment_id_chr" = "comment_id"))

stopifnot(nrow(org_flagged) == 9, !anyNA(org_flagged$verdict))

adj_summary <- org_flagged %>% count(verdict, target, name = "n")


# ---- 7. Console report -------------------------------------------------------

cat("\n", strrep("=", 78), "\n", sep = "")
cat("DISPUTE-MARKER LEXICON: paid vs organic comment corpora\n")
cat(strrep("=", 78), "\n\n", sep = "")

cat("Lexicon: ", nrow(lexicon), " marker families (",
    paste(sort(unique(lexicon$lang)), collapse = ", "), "); extension adds ",
    nrow(lexicon_ext), ".\n\n", sep = "")

cat("Denominators\n")
cat("  advertisement comments : ", nrow(ads), "\n", sep = "")
cat("  organic comments       : ", nrow(org),
    "  (in_corpus == 1, ids verified against comments_labeled.csv)\n\n", sep = "")

print(as.data.frame(contrast), digits = 4)

cat("\nPer-family hit counts\n")
fam_counts <- bind_rows(lexicon, lexicon_ext) %>%
  transmute(lang, family,
            hits_ads = map_int(pattern,
              ~ sum(str_detect(ads$txt, regex(.x, ignore_case = TRUE)))),
            hits_org = map_int(pattern,
              ~ sum(str_detect(org$txt, regex(.x, ignore_case = TRUE))))) %>%
  filter(hits_ads > 0 | hits_org > 0)
print(as.data.frame(fam_counts))

cat("\nFlagged ADVERTISEMENT comments, with independent stance code\n")
flagged_ads %>%
  transmute(idx, condition, stance, markers,
            text = str_trunc(comment_text, 78)) %>%
  as.data.frame() %>%
  print(right = FALSE)

cat("\nStance distribution of the flagged advertisement comments\n")
print(as.data.frame(stance_of_flagged))
cat("\nConcordance with the independent codes\n")
print(as.data.frame(concordance), digits = 4)

cat("\nFlagged ORGANIC comments (primary rule), with POST-HOC verdict\n")
org_flagged %>%
  transmute(src_org, markers, verdict, target, text = str_trunc(text, 60)) %>%
  as.data.frame() %>%
  print(right = FALSE)

cat("\nPost-hoc adjudication summary (does NOT replace the pre-specified count)\n")
print(as.data.frame(adj_summary))

cat("\nAdditional ORGANIC comments caught only by the sensitivity extension\n")
extra_org <- org %>% filter(flagged_ext, !flagged)
if (nrow(extra_org) == 0) {
  cat("  none\n")
} else {
  extra_org %>%
    transmute(src_org, markers_ext, text = str_trunc(text, 78)) %>%
    as.data.frame() %>%
    print(right = FALSE)
}


# ---- 8. Write outputs --------------------------------------------------------

bind_rows(lexicon %>% mutate(arm = "primary"),
          lexicon_ext %>% mutate(arm = "sensitivity")) %>%
  dplyr::select(arm, lang, family, pattern, why) %>%
  write_csv(here("output/tables/study2_expl_dispute_lexicon_terms.csv"))

contrast %>%
  write_csv(here("output/tables/study2_expl_dispute_lexicon_contrast.csv"))

bind_rows(
  flagged_ads %>%
    transmute(corpus = "advertisement", unit = as.character(idx),
              source = ad_name, stance_code = as.character(stance),
              markers, markers_ext, text = comment_text),
  org_flagged %>%
    transmute(corpus = "organic", unit = comment_id_chr,
              source = src_org, stance_code = NA_character_,
              markers, markers_ext, text,
              posthoc_verdict = verdict, posthoc_target = target,
              posthoc_reason = reason)
) %>%
  write_csv(here("output/tables/study2_expl_dispute_lexicon_flagged.csv"))


# ---- 9. Reproduction verdict against the chapter text -----------------------
# The chapter (Exploratory Analyses) states: 15 of 71 advertisement comments
# (21.1%) against 4 of 2,172 organic comments (0.18%), a difference of 20.9
# percentage points, 95% CI [13.1, 31.8].

claimed <- newcombe(15, 71, 4, 2172)
obs_ad  <- sum(ads$flagged); obs_org <- sum(org$flagged)
obs     <- newcombe(obs_ad, 71, obs_org, 2172)

cat("\n", strrep("=", 78), "\n", sep = "")
cat("REPRODUCTION VERDICT\n")
cat(strrep("=", 78), "\n", sep = "")
cat(sprintf("  claimed  : ads %2d/71 (%.1f%%)  organic %d/2172 (%.2f%%)  diff %.1f pp  CI [%.1f, %.1f]\n",
            15L, 15 / 71 * 100, 4L, 4 / 2172 * 100,
            claimed[1] * 100, claimed[2] * 100, claimed[3] * 100))
cat(sprintf("  observed : ads %2d/71 (%.1f%%)  organic %d/2172 (%.2f%%)  diff %.1f pp  CI [%.1f, %.1f]\n",
            obs_ad, obs_ad / 71 * 100, obs_org, obs_org / 2172 * 100,
            obs[1] * 100, obs[2] * 100, obs[3] * 100))
cat("\n  Advertisement numerator : ",
    if (obs_ad == 15) "REPRODUCES exactly (15/71)." else
      paste0("DEVIATES (", obs_ad, " vs 15)."), "\n", sep = "")
cat("  Organic numerator       : ",
    if (obs_org == 4) "REPRODUCES exactly (4/2172)." else
      paste0("DEVIATES (", obs_org, " vs 4). ",
             sum(org_flagged$verdict == "false_positive"),
             " of the ", obs_org,
             " are post-hoc false positives (see section 6b); ",
             sum(org_flagged$verdict == "genuine_dispute"),
             " survive inspection."), "\n", sep = "")
cat("  Interval implementation : reproduces the published CI exactly when fed\n",
    "                            the published counts, so the Newcombe code is\n",
    "                            not the source of any difference.\n", sep = "")

# ---- 8. Cluster bootstrap on the contrast -----------------------------------
# The Newcombe interval above treats every comment as an independent observation.
# Both corpora cluster: advertisement comments within 12 advertisements, organic
# comments within posts. Study 1 already applies a cluster bootstrap to its own
# support share one section earlier, so the same tool applies to the cross-corpus
# contrast.
#
# Procedure: resample CLUSTERS with replacement within each corpus, recompute both
# proportions and their difference on the resampled comments, take the percentile
# interval. Resampling clusters changes n from draw to draw, which is the point:
# it propagates the dependence the Newcombe interval ignores.

set.seed(2026)                       # same seed as the Study 1 cluster bootstrap
B <- 10000

boot_prop <- function(df, cluster_col, flag_col) {
  cl   <- split(df[[flag_col]], df[[cluster_col]])
  # sort(method = "radix") orders the cluster keys byte by byte, independent of the
  # machine's collation locale. split() alone leaves that order to LC_COLLATE, and
  # because the seed fixes which INDICES sample() draws, a different key order
  # produces a different resample and a different interval on another machine.
  keys <- sort(names(cl), method = "radix")
  draw <- sample(keys, length(keys), replace = TRUE)
  mean(unlist(cl[draw], use.names = FALSE))
}

boot_diff <- replicate(B, {
  100 * (boot_prop(ads, "ad_name", "flagged") - boot_prop(org, "Post_ID", "flagged"))
})

cluster_ci <- quantile(boot_diff, c(0.025, 0.975), names = FALSE)

dispute_cluster <- tibble(
  estimator    = c("Newcombe, comments independent", "Cluster bootstrap"),
  diff_pp      = round(c(contrast$diff_pp[1], mean(boot_diff)), 2),
  ci_lo        = round(c(contrast$diff_ci_lo[1], cluster_ci[1]), 2),
  ci_hi        = round(c(contrast$diff_ci_hi[1], cluster_ci[2]), 2),
  clusters_ad  = c(NA, n_distinct(ads$ad_name)),
  clusters_org = c(NA, n_distinct(org$Post_ID)),
  draws        = c(NA, B)
)

write_csv(dispute_cluster, here("output/tables/study2_expl_dispute_cluster_ci.csv"))
cat("\n--- Cluster bootstrap on the dispute contrast ---\n")
print(as.data.frame(dispute_cluster))

cat("\nWrote 4 tables to output/tables/ (prefix study2_expl_dispute_).\n")
