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


ggsave("images/biblio-concepts.png", biblio_concepts, dpi = 450,
       width = 7, height = 5)



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

small_biblio_works <- biblio_works[1:1000,]
hist(biblio_works$cited_by_count)
biblio_works |>
  ggplot() +
  aes(y = cited_by_count, x = 1) +
  geom_violin()

biblio_works |>
  count(so, sort = TRUE) |>
  drop_na(so) |>
  slice_max(n, n = 8) |>
  mutate(so = forcats::fct_reorder(so, n)) |>
  ggplot() +
  aes(x = n, y = so, fill = so) +
  geom_col() +
  scale_fill_viridis_d(option = "B") +
  guides(fill = "none") +
  labs(x = "Total citations", y = NULL) +
  coord_cartesian(expand = FALSE)

rel_venues <- biblio_works |>
  mutate(so = gsub("Journal of the|Journal of", "J.", so)) |>
  count(so) |>
  drop_na(so) |>
  slice_max(n, n = 8) |>
  pull(so)

# add "others" so
library(ggrepel)
biblio_journal <- biblio_works |>
  mutate(so = gsub("Journal of the|Journal of", "J.", so)) |>
  # add_count(so, name = "n_total") |>
  filter(so %in% rel_venues, publication_year < 2022) |>
  count(so, publication_year, sort = TRUE) |>
  mutate(
    so = as_factor(so) |> fct_relevel(rel_venues),
    label = if_else(publication_year == max(publication_year),
                    as.character(so), NA_character_)) |>
  # slice_max(n_total, n = 8) |>
  ggplot() +
  aes(x = publication_year, y = n, fill = so, color = so) +
  geom_area() +
  geom_text(aes(label = label, x = publication_year + 1),
            position = position_stack(vjust = 0.5),
            hjust = 0, na.rm = TRUE) +
  scale_y_continuous(expand = expansion(add = c(0, 0))) +
  scale_x_continuous(expand = expansion(add = c(0, 22.5))) +
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2") +
  labs(y = "Total works", x = NULL) +
  guides(fill = "none", color = "none")

ggsave("images/biblio-journals.png", biblio_journal,
       dpi = 450, height = 5, width = 10
       )

authors <- bind_rows(small_biblio_works$author) |>
  filter(author_position %in% c("first", "last"))

authors |>
  drop_na(institution_display_name) |>
  count(institution_display_name, sort = TRUE)
