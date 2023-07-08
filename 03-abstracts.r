library(tidyverse)
library(rvest)

fs::dir_create("data")

f <- fs::dir_ls("html/abstracts")
d <- tibble::tibble()

cat("Parsing", length(f), "abstracts...\n")
# note: could be written as a `map_dfr` call
for (i in f) {

  h <- read_html(i)

  d <- tibble::tibble(
    # numeric id in the abstract URL
    abstract_id = i,
    abstract_ref = html_node(h, "main h2") %>%
      html_text(),
    abstract_title = html_node(h, "main h1") %>%
      html_text(),
    abstract_text = html_nodes(h, "p.calibri") %>%
      html_text() %>%
      str_flatten(collapse = " "),
    # note: parsing the column below, but ditching it afterwards -- pairing
    # authors and affiliations is too much work here, let's do this by scraping
    # and parsing the authors' listing separately
    abstract_authors = html_nodes(h, xpath = "//h1[contains(@class, 'my-5')]/following::div[1]/div[1]/span/span") %>%
      html_text() %>%
      str_flatten_comma(),
    abstract_presenters = html_nodes(h, xpath = "//h1[contains(@class, 'my-5')]/following::div[1]/div[1]/span/span[contains(@class, 'underline')]") %>%
      html_text() %>%
      str_flatten_comma()
  ) %>%
    bind_rows(d)

}

# sanity check: 100% abstracts parsed
stopifnot(nrow(d) == length(f))

# drop unused columns
d <- select(d, -abstract_authors) %>%
  # minimal data cleaning
  mutate(
    abstract_id = str_extract(abstract_id, "\\d+"),
    abstract_text = str_squish(abstract_text)
  )

# sanity check: no missing abstract ids
stopifnot(str_detect(d$abstract_id, "\\d+"))

# sanity check: no duplicates (single row per abstract)
stopifnot(!duplicated(d$abstract_id))

# export ------------------------------------------------------------------

readr::write_tsv(d, "data/abstracts.tsv")

# kthxbye
