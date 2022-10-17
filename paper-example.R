#####
# oa_fetch works
library(openalexR)
works_df <- oa_fetch(
  entity = "works",
  type = "journal-article",
  host_venue.license = "cc-by",
  publication_date = "2022-08-15",
  concepts.id = "C70721500",
  verbose = TRUE
)
head(works_df$display_name)
paste0(gsub("^((\\w+\\W+){8}\\w+).*$", "\\1", head(works_df$display_name)), "...")

#####
# oa_fetch concepts

library(gghighlight)
library(openalexR)
library(dplyr)
library(cowplot)

theme_set(
  theme_classic() +
    theme(
      plot.background = element_rect(fill = "transparent", colour = NA),
      panel.background = element_rect(fill = "transparent", colour = NA),
      strip.background = element_rect(fill = NA, color = "grey20")
    )
)

library(openalexR)
concept_df <- oa_fetch(
  entity = "concepts",
  level = 1,
  ancestors.id = "C86803240", # Biology
  works_count = ">1000000",
  verbose = TRUE
)
journal_df <- oa_fetch(
  entity = "venues",
  works_count = ">300000",
  cited_by_count = ">20000",
  verbose = TRUE
) %>%
  distinct(display_name, .keep_all = TRUE) %>%
  select(jour = display_name, x_concepts) %>%
  tidyr::unnest(x_concepts) %>%
  filter(level == 0) %>%
  left_join(concept_abbrev) %>%
  mutate(abbreviation = gsub(" ", "<br>", abbreviation)) %>%
  tidyr::complete(jour, abbreviation, fill = list(score = 0)) %>%
  group_by(jour) %>%
  mutate(
    color = if_else(score > 10, "#1A1A1A", "#D9D9D9"), # CCCCCC
    label = paste0("<span style='color:", color, "'>", abbreviation, "</span>")
  )

p_concept <- concept_df %>%
  select(display_name, counts_by_year) %>%
  tidyr::unnest(counts_by_year) %>%
  filter(year < 2022) %>%
  ggplot() +
  aes(x = year, y = works_count, color = display_name) +
  facet_wrap(~display_name, ncol = 2) +
  geom_line(size = 0.7) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    x = NULL, y = "Works count",
    title = "Biology concept trends in recent years"
  ) +
  guides(color = "none") +
  gghighlight(max(works_count) > 99000)

p_journal <- journal_df %>%
  ggplot() +
  aes(fill = jour, y = score, x = abbreviation, group = jour) +
  facet_wrap(~jour, ncol = 3) +
  geom_hline(yintercept = c(45, 90), colour = "grey90", size = 0.2) +
  geom_segment(
    aes(x = abbreviation, xend = abbreviation, y = 0, yend = 100),
    color = "grey95"
  ) +
  geom_col(color = "grey20") +
  coord_polar(clip = "off") +
  theme_bw() +
  theme(
    plot.background = element_rect(fill = "transparent", colour = NA),
    panel.background = element_rect(fill = "transparent", colour = NA),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.text = element_blank(),
    axis.ticks.y = element_blank(),
    strip.clip = "off",
    strip.background = element_rect(fill = "grey90", color = NA)
  ) +
  ggtext::geom_richtext(
    aes(y = 120, label = label),
    fill = NA, label.color = NA, size = 3
  ) +
  scale_fill_brewer(palette = "Set1") +
  guides(fill = "none") +
  labs(y = NULL, x = "", title = "Journal clocks")
# p_journal
oa_fig <- plot_grid(p_concept, p_journal, ncol = 2, rel_widths = c(1, 1.7), labels = "AUTO")
# oa_fig
ggsave("images/oa-ex-1.png", oa_fig, height = 8, width = 13)


#####
# oa_snowball

# for reprex
set.seed(1234)
library(ggraph)
library(tidygraph)
library(openalexR)
snowball_docs <- oa_snowball(
  identifier = c("W1964141474", "W1963991285"),
  verbose = TRUE
)
ggraph(graph = as_tbl_graph(snowball_docs), layout = "stress") +
  geom_edge_link(aes(alpha = stat(index)), show.legend = FALSE) +
  geom_node_point(aes(fill = oa_input, size = cited_by_count), shape = 21)

# style ggraph
g_citation <- ggraph(graph = as_tbl_graph(snowball_docs), layout = "stress") +
  geom_edge_link(aes(alpha = stat(index)), show.legend = FALSE) +
  geom_node_point(aes(fill = oa_input, size = cited_by_count), shape = 21) +
  geom_node_label(aes(filter = oa_input, label = id), nudge_y = 0.1, size = 6) +
  scale_edge_width(range = c(0.1, 1.5), guide = "none") +
  scale_size(range = c(3, 10), guide = "none") +
  scale_fill_manual(values = c("#1A5878", "#C44237"), na.value = "grey", name = "") +
  theme_graph() +
  theme(legend.position = "bottom") +
  theme(
    plot.background = element_rect(fill = "transparent", colour = NA),
    panel.background = element_rect(fill = "transparent", colour = NA),
    strip.background = element_rect(fill = NA, color = "grey20")
  ) +
  guides(fill = "none")

g_citation
ggsave("images/citation-graph.png", g_citation)
