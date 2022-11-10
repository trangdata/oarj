Examples shown in the R Journal manuscript
================
2022-11-10

``` r
library(openalexR)
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6.9000     ✔ purrr   0.3.4     
    ## ✔ tibble  3.1.8          ✔ dplyr   1.0.10    
    ## ✔ tidyr   1.2.1          ✔ stringr 1.4.1     
    ## ✔ readr   2.1.2          ✔ forcats 0.5.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
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
```

## Bibliometrics concept

``` r
concept <- oa_fetch(
  entity = "concepts",
  identifier = "C178315738", # openalex id
  count_only = FALSE,
  verbose = FALSE
)

concept$description
```

    ## [1] "statistical analysis of written publications, such as books or articles"

``` r
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
```

    ##       relation                               id           display_name level
    ## 1     ancestor  https://openalex.org/C124101348            Data mining     1
    ## 2     ancestor  https://openalex.org/C161191863        Library science     1
    ## 3     ancestor  https://openalex.org/C136764020         World Wide Web     1
    ## 4     ancestor   https://openalex.org/C41008148       Computer science     0
    ## 5  equal level  https://openalex.org/C525823164         Scientometrics     2
    ## 6  equal level https://openalex.org/C2779455604          Impact factor     2
    ## 7  equal level https://openalex.org/C2778407487             Altmetrics     2
    ## 8  equal level  https://openalex.org/C521491914            Webometrics     2
    ## 9  equal level https://openalex.org/C2781083858  Scientific literature     2
    ## 10 equal level https://openalex.org/C2778805511               Citation     2
    ## 11 equal level   https://openalex.org/C95831776    Information science     2
    ## 12 equal level https://openalex.org/C2779172887               PageRank     2
    ## 13 equal level  https://openalex.org/C138368954            Peer review     2
    ## 14 equal level https://openalex.org/C2779810430 Knowledge organization     2
    ## 15 equal level https://openalex.org/C2780416505 Collection development     2
    ## 16  descendant  https://openalex.org/C105345328      Citation analysis     3
    ## 17  descendant https://openalex.org/C2778793908        Citation impact     3
    ## 18  descendant https://openalex.org/C2780378607           Informetrics     3
    ## 19  descendant https://openalex.org/C2778032371         Citation index     3
    ## 20  descendant   https://openalex.org/C83867959                 Scopus     3
    ## 21  descendant https://openalex.org/C2776822937 Bibliographic coupling     3
    ## 22  descendant https://openalex.org/C2779693592        Journal ranking     3
    ## 23  descendant   https://openalex.org/C45462083  Documentation science     3
    ## 24  descendant https://openalex.org/C2777765086            Co-citation     3
    ##        score
    ## 1         NA
    ## 2         NA
    ## 3         NA
    ## 4         NA
    ## 5  6.6193560
    ## 6  4.1035270
    ## 7  2.5396087
    ## 8  2.3026270
    ## 9  1.6163236
    ## 10 1.6110690
    ## 11 1.5750017
    ## 12 1.5363927
    ## 13 1.4112837
    ## 14 1.0037539
    ## 15 0.8137859
    ## 16 4.9036117
    ## 17 4.0405297
    ## 18 2.1396947
    ## 19 1.8888942
    ## 20 1.6536747
    ## 21 1.3375385
    ## 22 1.1321522
    ## 23 0.8473609
    ## 24 0.8002241

## Trends of biliometrics-related concepts

``` r
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
  geom_line(linewidth = 0.7) +
  labs(x = NULL, y = "Works count") +
  scale_y_log10() +
  scale_x_date(labels = scales::date_format("'%y")) +
  guides(color = "none") +
  gghighlight(use_direct_label = FALSE)
biblio_concepts
```

<img src="paper-examples_files/figure-gfm/unnamed-chunk-3-1.png" width="100%" />

``` r
ggsave("images/biblio-concepts.png", biblio_concepts,
  dpi = 450, width = 7, height = 5
)
```

## Bibliometrics papers

``` r
biblio_works <- oa_fetch(
  entity = "works",
  concept.id = "C178315738",
  abstract = TRUE,
  count_only = FALSE
)
```

``` r
biblio_works |>
  count(so) |>
  drop_na(so) |>
  slice_max(n, n = 8) |>
  pull(so)
```

    ## [1] "Scientometrics"                                                   
    ## [2] "Journal of the Association for Information Science and Technology"
    ## [3] "Social Science Research Network"                                  
    ## [4] "Journal of Informetrics"                                          
    ## [5] "PLOS ONE"                                                         
    ## [6] "Sustainability"                                                   
    ## [7] "Sci-Tech Information Development & Economy"                       
    ## [8] "Library Philosophy and Practice"

``` r
biblio_journal <- biblio_works |>
  add_count(so, name = "n_so") |>
  count(so, publication_year, n_so, sort = TRUE) |>
  drop_na(so) |>
  mutate(so_rank = dense_rank(desc(n_so))) |>
  filter(so_rank < 9, publication_year < 2022) |>
  mutate(
    so = gsub("Journal of the|Journal of", "J.", so) |>
      as_factor() |>
      fct_reorder(so_rank)
  ) |>
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
```

<img src="paper-examples_files/figure-gfm/biblio-journal-1.png" width="100%" />

``` r
ggsave("images/biblio-journals.png", biblio_journal,
  dpi = 450, height = 5, width = 10
)
```

``` r
knitr::knit_exit()
```
