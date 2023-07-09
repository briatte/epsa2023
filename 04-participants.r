library(tidyverse)
library(rvest)

fs::dir_create("data")

f <- fs::dir_ls("html/participants")

# NOTE -- we get to parse the participant pages in three different ways, to get
# the three roles we are interested in (presenter, chair, discussant), and then
# assemble the result in a single dataset; this catches everyone and produces a
# single affiliation per participant, including those who were only chairs or
# discussants, e.g. 'Sergio Acenso' (which contains TWO typos -- fixed later)

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

# due to some weird bug in the downloaded pages, the participants pages will
# sometimes include the same participant more than once, with one extreme case
# who repeats > 100 times (Hye Young You):
filter(count(d, author, sort = TRUE), n > 1)

# fix the issue, which removes ~ 800 rows
d <- distinct(d)

# affiliations are never missing for authors (presenters), ...
filter(d, role == "p", is.na(affiliation))

# ... but are sometimes missing for chairs and discussants
filter(d, role == "c", is.na(affiliation)) # n = 20 missing
filter(d, role == "d", is.na(affiliation)) # n = 29 missing

# a fair number of those are due to typos, e.g. 'Roma[i]n Lachat', or slightly
# different writings, e.g. 'Rasmus Pedersen', 'Rasmus Tue Pedersen' and 'Rasmus
# T. Pedersen' all designate a single individual

# small number of fixes based on manual inspection, fixing a few names and
# overwriting affiliations (quick and dirty, but that's what works best here):

d$affiliation[ d$author == "Hye Young You" ] <- "New York University, USA"
# also listed as Joan Barcelo Soler -- using shorter version
d$author[ str_detect(d$author, "Joan Barcel[oó]") ] <- "Joan Barceló"
d$affiliation[ d$author == "Joan Barceló" ] <- "New York University - Abu Dhabi, UAE"
d$affiliation[ d$author == "Joan Barceló" ] <- "New York University - Abu Dhabi, UAE"
d$affiliation[ d$author == "Johanna Ida Plenter" ] <- "Heinrich Heine University Düsseldorf, Germany"
d$author[ str_detect(d$author, "Jona [Dd]e Jong") ] <- "Jona de Jong"
# affil. lists 'EUI Italy' and 'EUI Netherlands' -- losing the latter
d$affiliation[ d$author == "Jona de Jong" ] <- "European University Institute, Italy"
d$author[ d$author == "Julie Nielsen" ] <- "Julie Hassing Nielsen"
d$affiliation[ d$author == "Julie Hassing Nielsen" ] <- "Lund University, Sweden"
d$author[ d$author == "Kerimcan Kavakli" ] <- "Kerim Can Kavakli"
d$affiliation[ d$author == "Kerim Can Kavakli" ] <- "Bocconi University, Italy"
d$author[ d$author == "Kristina Simonsen" ] <- "Kristina Bakkær Simonsen"
d$affiliation[ d$author == "Kristina Bakkær Simonsen" ] <- "Aarhus University, Denmark"
d$author[ d$author == "Lasse Lausten" ] <- "Lasse Laustsen"
d$affiliation[ d$author == "Lasse Laustsen" ] <- "Aarhus University, Denmark"
d$author[ d$author == "Leon Küstermann" ] <- "Leon David Küstermann"
d$affiliation[ d$author == "Leon David Küstermann" ] <- "European University Institute, Italy"
d$affiliation[ d$author == "Mads Dagnis Jensen" ] <- "Copenhagen Business School, Denmark"
d$affiliation[ d$author == "Martin Vinæs Larsen" ] <- "Aarhus University, Denmark"
d$author[ d$author == "Mattias Basedau" ] <- "Matthias Basedau"
d$affiliation[ d$author == "Matthias Basedau" ] <- "German Institute for Global and Area Studies, Germany"
# simplifying the name here...
d$author[ d$author == "Michał Gulczyński" ] <- "Michal Gulczynski"
d$affiliation[ d$author == "Michal Gulczynski" ] <- "Bocconi University, Italy"
d$author[ d$author == "Nelson Ruiz" ] <- "Nelson A Ruiz"
d$affiliation[ d$author == "Nelson A Ruiz" ] <- "University of Essex, United Kingdom"
d$author[ d$author == "Pavi Suryanarayan" ] <- "Pavithra Suryanarayan"
d$affiliation[ d$author == "Pavithra Suryanarayan" ] <- "London School of Economics, United Kingdom"
d$affiliation[ d$author == "Peter Thisted Dinesen" ] <- "University of Copenhagen, Denmark"
d$author[ d$author == "Raluca Pahontu" ] <- "Raluca L Pahontu"
d$affiliation[ d$author == "Raluca L Pahontu" ] <- "King's College London, United Kingdom"
d$author[ str_detect(d$author, "Rasmus (T\\. |Tue )?Pedersen") ] <- "Rasmus Tue Pedersen"
d$affiliation[ d$author == "Rasmus Tue Pedersen" ] <- "Danish Center for Social Science Research (VIVE), Denmark"
# going for the shorter form here (no real rule...)
d$author[ d$author == "Richard Traunmueller" ] <- "Richard Traunmüller"
d$affiliation[ d$author == "Richard Traunmüller" ] <- "University of Mannheim, Germany"
d$author[ d$author == "Roman Lachat" ] <- "Romain Lachat" # typo
d$affiliation[ d$author == "Romain Lachat" ] <- "SciencesPo Paris, France"
# simplifying name again, sorry...
d$author[ d$author == "Selim Erdem Aytaç" ] <- "Selim Erdem Aytac"
d$affiliation[ d$author == "Selim Erdem Aytac" ] <- "Koc University, Turkey"
d$author[ d$author == "Sergio Acenso" ] <- "Sergio Ascencio" # likely (double) typo
d$affiliation[ d$author == "Sergio Ascencio" ] <- "University of Essex, United Kingdom"
d$author[ d$author == "Staffan Kumlim" ] <- "Staffan Kumlin" # typo
d$affiliation[ d$author == "Staffan Kumlin" ] <- "University of Oslo, Norway"

# result:
filter(d, role == "c", is.na(affiliation)) # n = 8 missing
filter(d, role == "d", is.na(affiliation)) # n = 15 missing

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
