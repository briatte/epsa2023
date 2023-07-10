library(tidyverse)
library(ggraph)
library(tidygraph)

# [DRAFT] quick and dirty overview of main component -- we do things slightly
# differently than we did with `epsa2022`, in two ways: (1) we aggregate the
# weights at the end of the chain producing the edge list `e`, and (2) we show
# betweenness centrality instead of degree centrality

# very very draft, but sufficient to show that the program dataset works fine

d <- readr::read_tsv("data/program.tsv")

e <- filter(d, role == "p") %>%
  select(abstract_id, affiliation) %>%
  group_by(abstract_id) %>%
  filter(n() > 1) %>%
  group_split() %>%
  map(pull, affiliation) %>%
  # weight = 1 / number of organizations
  map_dfr(~ crossing(i = .x, j = .x, w = 1 / (length(.x) - 1))) %>%
  # de-duplicate, remove self-ties
  filter(i < j) %>%
  # aggregate weights over abstracts
  group_by(i, j) %>%
  summarise(w = sum(w))

# weights range 0.0625-5.3333
table(e$w)

# we go for betweenness centrality in this one, nicer results
tidygraph::as_tbl_graph(e, directed = FALSE) %>%
  tidygraph::activate(nodes) %>%
  mutate(
    wdegree = tidygraph::centrality_betweenness(weights = w),
    group = tidygraph::group_components(),
    # remove ' of ' and final country
    label = str_remove_all(name, "\\sof|,\\s.*?$"),
    label = if_else(
      str_count(label, "\\s") > 1,
      str_split(label, "\\s") %>%
        map(str_sub, 1, 1) %>%
        map_chr(str_c, collapse = ""),
      label
    ),
    label = if_else(wdegree > 4000, label, NA_character_) %>%
      str_remove("University") %>%
      str_squish()
  ) %>%
  filter(group == 1) %>%
  ggraph::ggraph(layout = "stress") +
  ggraph::geom_edge_link0() +
  ggraph::geom_node_point(aes(size = wdegree)) +
  ggraph::geom_node_label(aes(label = label)) +
  ggraph::theme_graph() +
  guides(size = "none")

ggsave("example-network.png", width = 8, height = 7)

# wip
