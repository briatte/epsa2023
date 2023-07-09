library(tidyverse)
library(rvest)

fs::dir_create("data")

f <- fs::dir_ls("html/participants")

# NOTE -- we get to parse the participant pages in three different ways, to get
# the three roles we are interested in (presenter, chair, discussant), and then
# assemble the result in a single dataset; this catches everyone and produces a
# single affiliation per participant, including those who were only chairs or
# discussants, e.g. Sergio Acenso

au <- tibble::tibble() # authors (presenters)
ch <- tibble::tibble() # chairs
di <- tibble::tibble() # discussants

cat("Parsing", length(f), "participant pages...\n")
for (i in f) {

  h <- read_html(i)

  # authors (presenters)
  d <- tibble::tibble(
    author = html_nodes(h, xpath = "//main/div[@class='relative']//h2") %>%
      html_text(),
    # single affiliation per author -- it looks like those have been 'flattened'
    # to a single string by the platform (e.g. Ruben Bach, Stefanie Bailer) or
    # by the participants themselves (e.g. Jack Baker)
    affiliation = html_nodes(h, xpath = "//h2/following::section[1]/div[1]") %>%
      html_text()
  )

  h <- html_nodes(h, xpath = "//h2/following::section[1]")

  au <- d %>%
    add_column(
      # list
      abstract_id = map(h, html_nodes, xpath = ".//span[text()='Author']/../../a") %>%
        map(html_attr, "href")
    ) %>%
    bind_rows(au)

  # chairs
  ch <- d %>%
    add_column(
      session_id = map(h, html_nodes, xpath = ".//span[text()='Symposium chair']/../../a[contains(@href,'session')]") %>%
        map(html_attr, "href")
    ) %>%
    bind_rows(ch)

  # discussants
  di <- d %>%
    add_column(
      session_id = map(h, html_nodes, xpath = ".//span[text()='Symposium discussant']/../../a[contains(@href,'session')]") %>%
        map(html_attr, "href")
    ) %>%
    bind_rows(di)

}

d <- bind_rows(
  add_column(tidyr::unnest(au, abstract_id) , role = "p", .after = 2),
  add_column(tidyr::unnest(ch, session_id)  , role = "c", .after = 2),
  add_column(tidyr::unnest(di, session_id)  , role = "d", .after = 2)
) %>%
  mutate(
    # 'domain' is the text of the affiliation icon
    affiliation = if_else(str_detect(affiliation, "^domain"), affiliation, NA) %>%
      str_remove("^domain"),
    abstract_id = str_extract(abstract_id, "\\d+$"),
    session_id = str_extract(session_id, "\\d+$")
  ) %>%
  arrange(author, affiliation, abstract_id, session_id)

# affiliations are never missing for authors (presenters), ...
filter(d, role == "p", is.na(affiliation))

# ... but are sometimes missing for chairs and discussants
filter(d, role == "c", is.na(affiliation)) # n = 20 missing
filter(d, role == "d", is.na(affiliation)) # n = 29 missing

# a fair number of those are due to typos, e.g. 'Roma[i]n Lachat', or slightly
# different writings, e.g. 'Rasmus Pedersen', 'Rasmus Tue Pedersen' and 'Rasmus
# T. Pedersen' all designate a single individual

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

# oh, and the many ways to write 'University'...
str_extract_all(d$affiliation, "Un.*?[td]\\w") %>%
  unlist() %>%
  unique() # imperfect and probably incomplete list

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
stopifnot(unique(as.character(a)) %in% unique(d$abstract_id[ d$role == "p" ]))

# export ------------------------------------------------------------------

readr::write_tsv(d, "data/participants.tsv")

# kthxbye
