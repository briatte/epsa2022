library(tidyverse)
library(ggraph)
library(tidygraph)

# [DRAFT] quick and dirty overview of main component

# -- parse abstracts -----------------------------------------------------------

library(rvest)
f <- fs::dir_ls("html/abstracts", glob = "*.html")

cat("Parsing", length(f), "abstracts...")

d <- map(f, read_html) %>%
  map_dfr(
    ~ tibble::tibble(
      # # detailed metadata
      # html_nodes(.x, "meta") %>%
      #   map_chr(html_attr, "content")
      panel = html_nodes(.x, xpath = "//a[contains(@href, 'session')]") %>%
        html_attr("href"),
      authors = html_nodes(.x, "meta[name='authors']") %>%
        html_attr("content"),
      affiliations = html_nodes(.x, "meta[name='affiliations']") %>%
        html_attr("content")
    ),
    .id = "abstract"
  ) %>%
  mutate(
    # authors have extra spaces, but are clean otherwise
    authors = str_squish(authors),
    # numeric identifiers but stored as character; `abstract` is 4-padded
    panel = str_remove_all(basename(panel), "\\D"),
    abstract = str_remove_all(basename(abstract), "\\D")
  ) %>%
  arrange(abstract)

cat("\n")

readr::write_tsv(d, "data/abstracts.tsv")

d <- readr::read_tsv("data/abstracts.tsv")

e <- str_split(d$affiliations, "(,\\s)?\\d\\.\\s") %>%
  map(str_subset, "\\w") %>%
  # weight = 1 / number of organizations
  map_dfr(~ crossing(i = .x, j = .x, w = 1 / (length(.x) - 1))) %>%
  # de-duplicate, remove self-ties
  filter(i < j)

# weights range 0.2-1
table(e$w)

tidygraph::as_tbl_graph(e, directed = FALSE) %>%
  tidygraph::activate(nodes) %>%
  mutate(
    wdegree = tidygraph::centrality_degree(weights = w),
    group = tidygraph::group_components(),
    label = str_remove_all(name, "\\sof"),
    label = if_else(
      str_count(label, "\\s") > 1,
      str_split(label, "\\s") %>%
        map(str_sub, 1, 1) %>%
        map_chr(str_c, collapse = ""),
      label
    ),
    label = if_else(wdegree > 3, label, NA_character_) %>%
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

ggsave("example-network.png", width = 7, height = 7)

# wip
