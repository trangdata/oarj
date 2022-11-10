library(openalexR)
library(tidyverse)
library(knitr)
library(gghighlight)
theme_set(
  theme_classic() +
    theme(
      plot.background = element_rect(fill = "transparent", colour = NA),
      panel.background = element_rect(fill = "transparent", colour = NA),
      strip.background = element_rect(fill = NA, color = "grey20")
    )
)

# load("data/oarj.rdata")

concept <- oa_fetch(
  entity = "concepts",
  identifier = "C178315738", # openalex id
  count_only = FALSE,
  verbose = FALSE
)

concept$description

ancestors <- concept$ancestors[[1]] |>
  mutate(relation = "ancestor")

equal_level <- concept$related_concepts[[1]] |>
  filter(level == 2) |>
  mutate(relation = "equal level")

descendants <- concept$related_concepts[[1]] |>
  filter(level == 3) |>
  mutate(relation = "descendant")

bind_rows(ancestors, equal_level, descendants) |>
  relocate(relation) |>
  select(-wikidata)


## Figure 2: Trends of biliometrics-related concepts
concept_df <- oa_fetch(
  entity = "concepts",
  identifier = c(concept$id, equal_level$id)
)

biblio_concepts <- concept_df |>
  select(display_name, counts_by_year) |>
  tidyr::unnest(counts_by_year) |>
  filter(year < 2022) |>
  mutate(year = as.Date(paste0("1jan", year), format = "%d%b%Y")) |>
  ggplot() +
  aes(x = year, y = works_count, color = display_name) +
  scale_color_viridis_d(option = "B", end = 0.8) +
  facet_wrap(~display_name) +
  geom_line(size = 0.7) +
  labs(x = NULL, y = "Works count") +
  scale_y_log10() +
  scale_x_date(labels = scales::date_format("'%y")) +
  guides(color = "none") +
  gghighlight(use_direct_label = FALSE)


ggsave("images/biblio-concepts.png", biblio_concepts,
  dpi = 450,
  width = 7, height = 5
)

## Get bibliometrics papers
# biblio_works <- oa_fetch(
#   entity = "works",
#   concept.id = "C178315738",
#   abstract = TRUE,
#   count_only = FALSE,
#   verbose = TRUE
# )
#
# saveRDS(biblio_works, "data/biblio-works.rds")
biblio_works <- readRDS("data/biblio-works.rds")

biblio_works |>
  count(so) |>
  drop_na(so) |>
  slice_max(n, n = 8) |>
  pull(so)

biblio_journal <- biblio_works |>
  add_count(so, name = "n_so") |>
  count(so, publication_year, n_so, sort = TRUE) |>
  drop_na(so) |>
  mutate(so_rank = dense_rank(desc(n_so))) |>
  filter(so_rank < 9, publication_year < 2022) |>
  mutate(so = as_factor(so) |> fct_reorder(so_rank)) |>
  complete(so, publication_year, fill = list(n = 0)) |>
  mutate(
    label = if_else(publication_year == max(publication_year),
      as.character(so), NA_character_
    )
  ) |>
  ggplot(aes(x = publication_year, y = n, fill = so)) +
  geom_area(alpha = 0.7, color = "white") +
  geom_text(aes(label = label, color = so, x = publication_year + 1),
    position = position_stack(vjust = 0.5),
    hjust = 0, na.rm = TRUE
  ) +
  scale_y_continuous(expand = expansion(add = c(0, 0))) +
  scale_x_continuous(
    expand = expansion(add = c(0, 22.5)),
    breaks = c(1980, 2000, 2020)
  ) +
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2") +
  labs(y = "Number of works", x = NULL) +
  theme_minimal() +
  theme(panel.grid.minor.y = element_blank()) +
  guides(fill = "none", color = "none")

biblio_journal
ggsave("images/biblio-journals.png", biblio_journal,
  dpi = 450, height = 5, width = 10
)

# authors <- bind_rows(small_biblio_works$author) |>
#   filter(author_position %in% c("first", "last"))

# authors |>
#   drop_na(institution_display_name) |>
#   count(institution_display_name, sort = TRUE)


## Most cited article
library(ggraph)
library(tidygraph)

seminal_works <- slice_max(biblio_works, cited_by_count, n = 8)
seminal_works$display_name

check = seminal_works[1:2,]


snowball_docs <- oa_snowball(
  identifier = seminal_works$id[1:2],
  verbose = TRUE
)

# style ggraph
g_citation <- ggraph(graph = as_tbl_graph(snowball_docs), layout = "stress") +
  geom_edge_link(alpha = 0.02) +
  geom_node_point(aes(fill = oa_input, size = cited_by_count), shape = 21) +
  scale_edge_width(range = c(0.1, 1.5), guide = "none") +
  scale_size(range = c(1, 3), guide = "none") +
  scale_fill_manual(values = c("#1A5878", "#C44237"), na.value = "grey", name = "") +
  theme_graph() +
  theme(legend.position = "bottom") +
  theme(
    plot.background = element_rect(fill = "transparent", colour = NA),
    panel.background = element_rect(fill = "transparent", colour = NA),
    strip.background = element_rect(fill = NA, color = "grey20")
  ) +
  guides(fill = "none") +
  geom_node_label(aes(filter = oa_input, label = id), nudge_y = 0.2, size = 5)

ggsave("images/citation-graph.png", g_citation)
# beepr::beep()

save.image("data/oarj.rdata")
