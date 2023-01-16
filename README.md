# oarj
Examples of openalexR submitted to R Journal

```{r}
df_au<- do.call(rbind.data.frame, df$author)
```
We can obtain information on the Most relevant Authors
```{r}
MRAuthors <- df_au |>
count(au_display_name) |>
drop_na(au_display_name) |>
slice_max(n, n = 10) |>
pull(au_display_name)
MRAuthors
```


```{r}
mra <- df_au |>
  count(au_display_name) |>
  drop_na(au_display_name) |>
  slice_max(n, n = 10) |>
  mutate(au_display_name = forcats::fct_reorder(au_display_name, n)) |>
  ggplot() +
  aes(y = n, x = au_display_name) +
  geom_segment(
    aes(x=au_display_name, xend=au_display_name, y=0, yend=n), 
    color = "#a3ad62",
  ) +
  geom_point(color="#d46780", size=4) +
  theme_ipsum() +
  coord_flip() +
  theme(legend.position="none") +
  xlab("") +
  ylab("Articles") +
  theme(
    plot.background = element_rect(fill = "transparent", colour = NA),
    panel.background = element_rect(fill = "transparent", colour = NA),
    strip.background = element_rect(fill = NA, color = "grey20")
  )
```

We can obtain information on the Most relevant Institutions
```{r}
MRInstitutions <- df_au |>
count(institution_display_name) |>
drop_na(institution_display_name) |>
slice_max(n, n = 10) |>
pull(institution_display_name)
MRInstitutions
```

```{r}
mri <- df_au |>
  count(institution_display_name) |>
  drop_na(institution_display_name) |>
  slice_max(n, n = 10) |>
  mutate(institution_display_name= forcats::fct_reorder(institution_display_name, n)) |>
  ggplot() +
  aes(y = n, x = institution_display_name) +
  geom_segment(
    aes(x=institution_display_name, xend=institution_display_name, y=0, yend=n), 
    color = "#a3ad62",
  ) +
  geom_point(color="#d46780", size=4) +
  theme_ipsum() +
  coord_flip() +
  theme(legend.position="none") +
  xlab("") +
  ylab("Articles") +
  theme(
    plot.background = element_rect(fill = "transparent", colour = NA),
    panel.background = element_rect(fill = "transparent", colour = NA),
    strip.background = element_rect(fill = NA, color = "grey20")
  )
```

We can obtain information on the Most cited works

```{r}
df_1 <- df %>% show_works()
seminal_works <- slice_max(df, cited_by_count, n = 10)
seminal_works$display_name
```

```{r}
seminal_works$cited_by_count
```


snowball_docs
```{r}
snowball_docs <- (oa_snowball(
identifier = c("W2150220236", "W2120109270"),
verbose = TRUE
))
```


```{r}
snow_graph_1 <- as_tbl_graph(snowball_docs)
snowball_small <- snowball_docs |>
  as_tbl_graph() |>
  slice(1:500)
g_citation <- ggraph(graph = snow_graph_1, layout = "stress") +
  geom_edge_link(alpha = 0.02) +
  geom_node_point(
    data = ~ filter(.x, !oa_input),
    mapping = aes(size = cited_by_count),
    fill = "#a3ad62",
    shape = 21, color = "white"
  ) +
  geom_node_point(
    data = ~ filter(.x, oa_input),
    mapping = aes(size = cited_by_count),
    fill = "#d46780",
    shape = 21, color = "white"
  ) +
  theme_graph() +
  theme(legend.position = "bottom") +
  theme(
    plot.background = element_rect(fill = "transparent", colour = NA),
    panel.background = element_rect(fill = "transparent", colour = NA),
    strip.background = element_rect(fill = NA, color = "grey20")
  ) +
  guides(fill = "none", size = "none") +
  geom_node_label(aes(filter = oa_input, label = work_labels[id]), nudge_y = 0.2, size = 3)
g_citation
```


```{r}
ngrams_data <- oa_ngrams(c("W2150220236", "W2120109270"), options("oa_ngrams.message.curlv5" = FALSE))
```

```{r}
first_paper_ngrams <- ngrams_data$ngrams[[1]]
second_paper_ngrams <- ngrams_data$ngrams[[2]]

top_10_1 <- first_paper_ngrams %>%
  slice_max(ngram_count, n = 10, with_ties = FALSE)
top_10_1 
top_10_2 <- second_paper_ngrams %>%
  slice_max(ngram_count, n = 10, with_ties = FALSE)
top_10_2
```


