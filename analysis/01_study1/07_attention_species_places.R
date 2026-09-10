# =============================================================================
# 01_study1/07_attention_species_places.R
#
# WHERE THIS BELONGS
#   In the thesis: Chapter 3, Results: Species, Places, and Institutional Attention
#   Produces: Figure 4, Table B4
#   Status: Exploratory. Study 1 carries no preregistration at all, so every
#   association here is descriptive and none of it tests a hypothesis that was
#   stated in advance.
#
# Study 1 (content analysis, PURELY EXPLORATORY, no preregistration): who and what
# gets attention. Three descriptive strands:
#   1. Species attention vs IUCN threat status (does coverage track endangerment?)
#   2. Geographic focus (which places the corpus foregrounds)
#   3. The org-to-org @mention network (hubs, communities, external partner web)
#
# Purely exploratory: effect sizes with 95% CIs, descriptive language, no test/
# confirm framing. The species-threat join uses species-level IUCN Red List
# categories (subspecies collapsed) - read as INDICATIVE, not definitive.
#
# Inputs (data/study1/features/, from the Python entity pipeline c5_entity_features.py):
#   entities_species_summary.csv  one row per species (attention counts + IUCN)
#   entities_species_long.csv     one row per species-in-post mention
#   entities_place_summary.csv    place mentions (gazetteer + spaCy)
#   entities_org_edges.csv        @mention edges (org -> mentioned handle)
#   entities_ner_orgs_long.csv    spaCy-detected ORG names (external partners)
# Output: output/tables/attn_species_threat.csv, attn_species_threat_rho.csv,
#         attn_places.csv (+ APA .docx), attn_org_centrality.csv,
#         attn_org_bridge_paths.csv;
#         output/figures/fig_attn_species_threat.png/.pdf     (Figure 4)
# =============================================================================

# igraph builds the @mention network and tidygraph holds it as a table of nodes and
# edges. The network itself is not drawn, the one saved figure is the species panel.
pacman::p_load(here, tidyverse, igraph, tidygraph, ggraph, flextable, officer)
list.files(here("src"), full.names = TRUE) %>% walk(source)

# set.seed(42) fixes the RNG so the percentile-bootstrap CI (species-threat) and the
# stochastic community detection + force-directed network layout reproduce exactly.
set.seed(42)

# The extracted-entity tables produced by the Python NER / entity pipeline:
species_attn <- read_csv(here("data/study1/features/entities_species_summary.csv"), show_col_types = FALSE)  # one row per species
species_long <- read_csv(here("data/study1/features/entities_species_long.csv"),    show_col_types = FALSE)  # one row per species-in-post
places       <- read_csv(here("data/study1/features/entities_place_summary.csv"),   show_col_types = FALSE)  # place mentions
org_mentions <- read_csv(here("data/study1/features/entities_org_edges.csv"),       show_col_types = FALSE)  # @mention edges
ner_orgs     <- read_csv(here("data/study1/features/entities_ner_orgs_long.csv"),   show_col_types = FALSE)  # spaCy ORG names

iucn_levels <- c("LC", "NT", "VU", "EN", "CR")   # IUCN Red List categories, least -> most threatened


# ---- 1. Species attention vs IUCN threat ------------------------------------
# Turn the IUCN status into an ordered factor and a 0..4 rank (LC = 0 ... CR = 4) so
# endangerment can be correlated with attention.
species_attn <- species_attn %>%
  mutate(iucn_status = factor(iucn_status, levels = iucn_levels),
         iucn_rank   = as.integer(iucn_status) - 1L)

c(n_species = nrow(species_attn), n_species_post_rows = sum(species_attn$n_posts))
# 29 distinct species detected across 426 species-post rows.

# boot_spearman: Spearman rho on ranks (attention counts are highly skewed; threat is
# ordinal) with a percentile bootstrap 95% CI over species. Returns c(rho, lo, hi).
boot_spearman <- function(x, y, n_resamples = 3000) {
  ok <- !is.na(x) & !is.na(y); x <- x[ok]; y <- y[ok]
  observed <- suppressWarnings(cor(x, y, method = "spearman"))          # observed rank correlation
  resampled <- replicate(n_resamples, {
    i <- sample(length(x), replace = TRUE)                              # resample species with replacement
    suppressWarnings(cor(x[i], y[i], method = "spearman"))
  })
  c(rho = observed,
    lo  = quantile(resampled, .025, na.rm = TRUE),
    hi  = quantile(resampled, .975, na.rm = TRUE))
}

attn_threat_rho <- boot_spearman(species_attn$n_posts, species_attn$iucn_rank)
round(attn_threat_rho, 3)

# The bootstrap carries the interval but returns no test statistic, and the Results
# quote the coefficient in prose. cor.test supplies the exact p on the same ranks,
# so the sentence can name its n and its p as the other correlations in this
# chapter do (APA 7 section 6.44).
attn_threat_test <- suppressWarnings(
  cor.test(species_attn$n_posts, species_attn$iucn_rank, method = "spearman"))
attn_threat_stats <- tibble(
  quantity = c("n_species", "rho", "ci_low", "ci_high", "S", "p"),
  value    = c(nrow(species_attn), attn_threat_rho[["rho"]],
               attn_threat_rho[[2]], attn_threat_rho[[3]],
               as.numeric(attn_threat_test$statistic), attn_threat_test$p.value))
write_csv(attn_threat_stats, here("output/tables/attn_species_threat_rho.csv"))
print(attn_threat_stats)
# rho = .490 [.162, .734], S = 2072.4, p = .007 on 29 species.
# Spearman(attention posts, IUCN rank) = .490 [.162, .734]: a moderate positive
# association on a wide interval, so it is noisy and not to be over-read. More
# endangered species get somewhat more coverage, but the pattern is dominated by
# charismatic Vulnerable megafauna rather than by threat level per se.

# Attention broken down by IUCN category: count DISTINCT posts per category, as a %.
attn_by_threat <- species_long %>%
  distinct(Post_ID, iucn_status) %>%
  count(iucn_status) %>%
  mutate(iucn_status = factor(iucn_status, levels = iucn_levels)) %>%
  arrange(iucn_status) %>%
  mutate(pct = round(100 * n / sum(n)))

attn_by_threat
# Distinct-post attention by category: LC 16%, NT 5%, VU 47%, EN 21%, CR 11%. The
# Vulnerable band (charismatic megafauna) absorbs nearly half of all species attention.

species_threat_table <- species_attn %>% arrange(desc(n_posts), species)
write_csv(species_threat_table, here("output/tables/attn_species_threat.csv"))

# Flag high-attention species carried by a SINGLE org: the "attention" is one account's
# beat, not corpus-wide interest, so it should not be over-read.
single_org_species <- species_attn %>%
  filter(n_orgs == 1, n_posts >= 10) %>%
  arrange(desc(n_posts)) %>%
  dplyr::select(species, iucn_status, n_posts, n_orgs)

single_org_species
# Giraffe (VU, 79 posts, 1 org): the single most-posted species is entirely one
# organisation's beat, so its apparent dominance is an organisational artefact.


# ---- 2. Geographic focus ----------------------------------------------------
# Most-mentioned places, restricted to gazetteer matches (verified place names, not
# free-text NER guesses), sorted by how many posts mention them.
# Ties are broken by name so the row order is stable. Without the second key the
# order among equal counts depends on the input order and the file stops being
# byte-comparable between runs, which defeats the manifest check.
places_gazetteer <- places %>% filter(source == "gazetteer") %>%
  arrange(desc(n_posts), place)

head(places_gazetteer, 12)
# Namibia (320 posts) dominates as the umbrella reference, then Windhoek (49), Gobabeb
# (37), the Namib (32), Okonjima (27) and Etosha (23); coverage concentrates on a few
# flagship sites and field stations rather than spreading evenly across the country.

write_csv(places_gazetteer, here("output/tables/attn_places.csv"))
save_apa_table(places_gazetteer, "attn_places",
               title = "Table. Geographic Focus (Gazetteer-Matched Place Mentions)",
               note  = "n_posts = posts mentioning the place. Restricted to verified gazetteer matches.",
               digits = 0)


# ---- 3. Org-to-org @mention network -----------------------------------------
# Directed network of @mentions BETWEEN corpus orgs (is_own_org == 1 = the mentioned
# handle is itself one of the sampled organisations). Edge weight = mention count.
mention_edges <- org_mentions %>%
  filter(is_own_org == 1) %>%
  transmute(from = src_org, to = mentioned, weight)
mention_net <- graph_from_data_frame(mention_edges, directed = TRUE)

c(nodes = gorder(mention_net), edges = gsize(mention_net))
# 17 within-corpus @mention edges spanning 9 org nodes: a small, hub-centred partner
# web rather than a densely cross-referencing community.

# Centrality per node. degree = how many distinct orgs mention it / it mentions;
# strength = weighted incoming (total times mentioned = amplification received);
# betweenness = how often it sits on shortest directed paths in the UNWEIGHTED
# network. Mention frequency is tie strength, not distance, so weights = NA is
# explicit. Passing raw mention counts as path lengths would make frequent mentions
# count as longer, weaker routes and reverse their intended meaning.
centrality <- tibble(
  org         = V(mention_net)$name,
  in_deg      = degree(mention_net, mode = "in",  loops = FALSE),
  out_deg     = degree(mention_net, mode = "out", loops = FALSE),
  in_strength = strength(mention_net, mode = "in", weights = E(mention_net)$weight),
  # betweenness [not reported]: in-degree, out-degree and in-strength carry into
  # the reported table, this column does not.
  betweenness = round(betweenness(mention_net, directed = TRUE, weights = NA), 1)
) %>% arrange(desc(in_strength))

head(centrality, 8)
# The Ministry of Environment (in_strength = 20, in_deg = 5) is the most-amplified and
# most-mentioned node; Namibia Nature Foundation sits highest on betweenness (24.5), the
# key broker bridging the smaller accounts. A clear hub-and-spoke, not a mesh.

write_csv(centrality, here("output/tables/attn_org_centrality.csv"))

# Translate the Foundation's raw betweenness score into a count a reader can follow.
# Exclude pairs that start or end at the Foundation, then inspect every ordered pair
# of remaining organizations. A pair enters the denominator only where a directed
# route exists. "Any" preserves ties: if two routes are equally short and one passes
# through the Foundation, that pair counts once here while contributing 0.5 to the
# conventional betweenness score.
# ---- Namibia Nature Foundation bridge paths [not reported] -------------------
# Counts the organization pairs that reach each other only through the Foundation.
bridge_org <- "namibia_nature_foundation"
other_orgs <- setdiff(V(mention_net)$name, bridge_org)

bridge_pairs <- map_dfr(other_orgs, function(from_org) {
  map_dfr(setdiff(other_orgs, from_org), function(to_org) {
    paths <- all_shortest_paths(mention_net, from = from_org, to = to_org,
                                mode = "out", weights = NA)$res
    if (length(paths) == 0) return(tibble())
    via_bridge <- vapply(paths, function(path) bridge_org %in% as_ids(path), logical(1))
    tibble(from = from_org, to = to_org,
           n_shortest_paths = length(paths),
           n_via_bridge = sum(via_bridge))
  })
})

bridge_path_summary <- tibble(
  org = bridge_org,
  reachable_directed_pairs = nrow(bridge_pairs),
  pairs_with_any_shortest_path_via = sum(bridge_pairs$n_via_bridge > 0),
  pairs_with_all_shortest_paths_via = sum(bridge_pairs$n_via_bridge == bridge_pairs$n_shortest_paths),
  unweighted_betweenness = centrality$betweenness[centrality$org == bridge_org]
)

stopifnot(bridge_path_summary$reachable_directed_pairs == 37,
          bridge_path_summary$pairs_with_any_shortest_path_via == 26,
          bridge_path_summary$unweighted_betweenness == 24.5)
write_csv(bridge_path_summary, here("output/tables/attn_org_bridge_paths.csv"))
# Of 37 reachable pairs, 26 pass through it on at least one shortest route,
# matching its betweenness of 24.5.
# -> one account holds the network together, a single point of failure in an
#    already thin web. Its plain share of the tagging relationships is reported,
#    10 of 17.

# ---- Community structure in the mention network [not reported] ---------------
# Does the tagging network fall into subgroups? Walktrap on the undirected graph,
# with modularity as how cleanly it splits (0 = not at all).
mention_net_undirected <- as_undirected(mention_net, mode = "collapse",
                                        edge.attr.comb = list(weight = "sum"))
communities <- cluster_walktrap(mention_net_undirected,
                                weights = E(mention_net_undirected)$weight)

c(communities = length(communities), modularity = round(modularity(communities), 2))
# Three communities, modularity .01.
# -> no clean partition, the linked accounts form one connected core rather than
#    rival camps. Reach and direction are reported instead of groups.


# ---- 4. Named partners and funders [not reported] ----------------------------
# External named orgs (partners / funders) mentioned in captions. Drop the corpus's own
# handles and a generic-token stoplist, keep names > 3 chars mentioned >= 4 times.
corpus_handles <- unique(c(mention_edges$from, mention_edges$to))              # the corpus orgs (exclude)
noise_tokens   <- c("wwf", "the", "namibia", "ig", "instagram", "ministry", "reels", "insta")
partner_mentions <- ner_orgs %>%
  mutate(org_text = str_squish(org_text)) %>%                                 # tidy whitespace
  filter(str_length(org_text) > 3, !str_to_lower(org_text) %in% noise_tokens) %>%
  count(org_text, sort = TRUE) %>%
  filter(n >= 4)                                                              # names mentioned >= 4 times

head(partner_mentions, 15)
# EHRA (45), AfriCat (27), Gobabeb (27), Cheetah Conservation Fund (22), MEFT (20).
# -> a standing partner web of field NGOs, research stations and government bodies,
#    wider than the set of accounts that tag one another. The tagging network
#    itself is reported.


# ---- Figures ----------------------------------------------------------------

# Species attention vs IUCN threat, bars shaded by threat status on a viridis scale
# (perceptually uniform, colourblind-safe). drop = FALSE keeps all 5 IUCN categories in
# the legend so the ordinal scale reads even where a category has no bar.
# Cut at >= 2 posts rather than a top-n slice: slice_max(n = 20) landed on 23 bars
# through ties, which is not a rule a figure note can state. The 6 single-post species
# are named in the note instead.
fig_attn_species_threat <- species_attn %>%
  filter(n_posts >= 2) %>%
  mutate(species = fct_reorder(species, n_posts)) %>%
  ggplot(aes(species, n_posts, fill = iucn_status)) +
  geom_col(width = .75) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +   # bars sit exactly on 0
  scale_fill_viridis_d(option = "viridis", drop = FALSE, name = "IUCN") +
  labs(x = NULL, y = "Posts Mentioning Species") +
  theme_apa()

fig_attn_species_threat
save_apa(fig_attn_species_threat, "fig_attn_species_threat", width = apa_width_full, height = 6)

# Geographic focus: top-14 gazetteer places by post count, shaded by place type.
set.seed(42)   # stress layout is deterministic given a seed
# Handle -> full org name, so nodes show "Ministry of Environment" not "meft_namibia".
org_names <- read_csv(here("data/study1/features/org_feature_matrix.csv"),
                      show_col_types = FALSE) %>%
  dplyr::select(Handle, Actor_Name)
mention_graph <- as_tbl_graph(mention_net) %>%
  mutate(community   = as.factor(membership(communities)[name]),   # walktrap cluster id per node
         in_mentions = centrality_degree(mode = "in"),             # times mentioned = node size
         # match the node key (an Instagram handle) to its display name; fall back to a
         # tidied key if a handle is somehow missing from the lookup.
         label = org_names$Actor_Name[match(name, org_names$Handle)],
         label = coalesce(label, str_to_title(str_replace_all(name, "_", " "))))

