library(tidyverse)
library(rvest)

fs::dir_create("data")

f <- fs::dir_ls("html/authors")
d <- tibble::tibble()

# IMPORTANT -- the authors pages also list individuals who were only chairs or
# discussants, but given how we do things below (pairing authors to papers on
# which they are listed as authors), those individuals are missing from the
# result (which is fine, since we get chairs and disc. from parsing sessions),
# e.g. Sergio Acenso

cat("Parsing", length(f), "author pages...\n")
# note: could be written as a `map_dfr` call
for (i in f) {

  h <- read_html(i)

  d <- tibble::tibble(
    author = html_nodes(h, xpath = "//main/div[@class='relative']//h2") %>%
      html_text(),
    # single affiliation per author -- it looks like those have been 'flattened'
    # to a single string by the platform (e.g. Ruben Bach, Stefanie Bailer) or
    # by the participants themselves (e.g. Jack Baker)
    affiliation = html_nodes(h, xpath = "//h2/following::section[1]/div[1]") %>%
      html_text(),
    # list
    abstract_id = html_nodes(h, xpath = "//h2/following::section[1]") %>%
      map(html_nodes, xpath = ".//span[text()='Author']/../../a") %>%
      map(html_attr, "href")
  ) %>%
    bind_rows(d)

}

d <- d %>%
  tidyr::unnest(abstract_id) %>%
  mutate(
    # 'domain' is the text of the affiliation icon
    affiliation = if_else(str_detect(affiliation, "^domain"), affiliation, NA) %>%
      str_remove("^domain"),
    abstract_id = str_extract(abstract_id, "\\d+$")
  )

# affiliations are never missing for authors
stopifnot(!is.na(d$affiliation))

# multiple affiliations are probably those with a '.' in them -- although many
# in those below are just two ways of writing the same single affiliation
d$affiliation %>%
  unique() %>%
  # remove false positives, e.g. "St. Louis" or "St. Gallen"
  str_replace_all(" St\\. ", " St ") %>%
  str_subset("\\.") %>%
  sort()

# ... and of course, there are also cases with lots of different affiliations
mutate(d, affiliation = str_replace_all(affiliation, " St\\. ", " St ")) %>%
  filter(str_count(affiliation, "\\.") > 1) %>%
  pull(affiliation) %>%
  unique()

# export to allow for some light manual fixing
f <- "data/affiliation-fixes.tsv"
if (!fs::file_exists(f)) {

  mutate(d, affiliation = str_replace_all(affiliation, " St\\. ", " St ")) %>%
    filter(str_count(affiliation, "\\.") > 0) %>%
    select(author, affiliation) %>%
    distinct() %>%
    mutate(affiliation_fix = affiliation) %>%
    arrange(author, affiliation) %>%
    readr::write_tsv(f)

}

# apply fixes -- [NOTE] stopped at Joaquín Rozas-Bugueño
d <- readr::read_tsv(f, col_types = "ccc") %>%
  right_join(d, by = c("author", "affiliation")) %>%
  mutate(affiliation = if_else(!is.na(affiliation_fix), affiliation_fix, affiliation)) %>%
  select(-affiliation_fix)

cat(f, ":", n_distinct(d$affiliation), "affiliations (post-fixes)\n")

# sanity check: we have authors for all abstracts
a <- read_tsv("data/abstracts.tsv", col_types = cols())$abstract_id
stopifnot(unique(as.character(a)) %in% unique(d$abstract_id))

# export ------------------------------------------------------------------

readr::write_tsv(d, "data/authors.tsv")

# kthxbye
