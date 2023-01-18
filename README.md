# oarj
Examples of openalexR submitted to R Journal

We can obtain information on the Most relevant Journal

```{r}
Venues <- df |>
count(so) |>
drop_na(so) |>
slice_max(n, n = 8) |>
pull(so)

MRV<- df |>
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
  coord_cartesian(expand = FALSE)+
  theme(
      plot.background = element_rect(fill = "transparent", colour = NA),
      panel.background = element_rect(fill = "transparent", colour = NA),
      strip.background = element_rect(fill = NA, color = "grey20")
    )
MRV
```


```{r}

biblio_authors <- do.call(rbind.data.frame, df$author)
```

We can obtain information on the Most relevant Authors

```{r}
Authors <- biblio_authors  |>
count(au_display_name) |>
drop_na(au_display_name) |>
slice_max(n, n = 10) |>
pull(au_display_name)
Authors

MRA <- biblio_authors  |>
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
Institutions <- biblio_authors  |>
count(institution_display_name) |>
drop_na(institution_display_name) |>
slice_max(n, n = 10) |>
pull(institution_display_name)
Institutions

MRI <- df_au |>
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
seminal_works <- df %>% show_works()
seminal_works <- slice_max(df, cited_by_count, n = 10)
info_seminal_works <- seminal_works%>%
select(.data$publication_year, .data$display_name, .data$so, .data$cited_by_count)

info_seminal_works<- as.data.frame(info_seminal_works)
info_seminal_works
```
N-grams

```{r}
ngrams_data <- oa_ngrams(df$id, options("oa_ngrams.message.curlv5" = TRUE), verbose = TRUE)
save(ngrams_data,file="ngrams_data.rdata")

```

```{r}
df_ngrams<- do.call(rbind.data.frame, ngrams_data$ngrams)
df_ng<- filter(ngrams_data, ngram_tokens == 2)
df_ngr<-arrange(df_ng, desc(ngram_count))
df_n<-df_ngr[nchar(df_ngr$ngram)>5,]
```

```{r}

top_10 <- df_n %>%
  slice_max(ngram_count, n = 10, with_ties = FALSE)
top_10

treemap <- ggplot(top_10, aes(area = ngram_count, fill = ngram)) +
  geom_treemap()

ggsave("treemap.png", treemap,
  height = 5, width = 8,
)
```
snowball_docs

```{r}
sb_docs <- oa_snowball(
identifier = c("W2150220236", "W2120109270", "W2755950973"),
citing_filter = list(from_publication_date = "2022-01-01"),
verbose = TRUE
)
```

```{r}
sg_1 <- as_tbl_graph(sb_docs)
```
```{r}
AU <- sb_docs$nodes %>% 
  #filter(id %in% c("W2150220236", "W2120109270", "W2755950973")) %>% 
  select(author)
AU <- unlist(AU,recursive = FALSE)
AU <- unlist(lapply(AU, function(l){
  paste(l$au_display_name,collapse=";")
}))

g_citation <- ggraph(graph = sg_1, layout = "stress") +
  geom_edge_link(color = "grey60", alpha = 0.30, show.legend = FALSE) +
  #geom_edge_link(alpha = 0.02) +
  geom_node_point(aes(fill = oa_input, size = cited_by_count), shape = 21) +
  scale_edge_width(range = c(0.1, 1.5), guide = "none") +
  scale_size(range = c(1, 3), guide = "none") +
  scale_fill_manual(values = c("#9DB9F1", "#4479E4"), na.value = "grey", name = "") +
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
    strip.background = element_rect(fill = "transparent", color = NA)
  ) +
  guides(fill = "none") +
  #geom_node_label(aes(filter = oa_input, label = id), nudge_y = 0.2, size = 5) +
  guides(fill = "none", size = "none") +
  geom_node_label(aes(filter = oa_input, label = AU), nudge_y = 0.2, size = 3)
g_citation
```
```{r}
ggsave("citation-graph.png", g_citation,
  height = 5, width = 8,
)
```


