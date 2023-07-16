# code recycled from `epsa2021/02-sessions.r`, although recycling code from
# `epsa2019` would have been smarter (same conference platform)

library(tidyverse)
library(rvest)

fs::dir_create("data")

# chair(s) and discussant(s) with affil.
# https://virtual.oxfordabstracts.com/#/event/3738/session/53485

# [!!] chair sometimes missing, e.g.
# https://virtual.oxfordabstracts.com/#/event/3738/session/53462

# [!!] multiple discussants, e.g.
# https://virtual.oxfordabstracts.com/#/event/3738/session/70694
# html_nodes(h, xpath = "//h3[text()='Discussants']/following::div[1]//span")

# using `ifelse` since `if_else` won't allow that behaviour
# https://github.com/tidyverse/dplyr/issues/6879
na_if_empty <- \(x) ifelse(length(x), x, NA)

f <- fs::dir_ls("html/sessions")
d <- tibble::tibble()
a <- tibble::tibble()

cat("Parsing", length(f), "sessions...\n")
for (i in f) {

  h <- read_html(i)

  # NOTE -- we collect chairs and discussants, but collect that information
  # again in `04-participants.r` and later use that source for reasons detailed
  # in that script (unified affiliations)

  # sessions (1-2 rows, depending on discussants)
  d <- tibble::tibble(
    # numeric id of the session URL (URL-based)
    # different from internal id, which is `html_nodes(h, "header h2")`
    session_id = i,
    # catches session_title on special panels
    session_ref = html_text(html_nodes(h, "h1")[[2]]),
    session_track = html_nodes(h, xpath = "//span[text()='Track']/..") %>%
      html_text() %>%
      na_if_empty(),
    session_type = html_nodes(h, xpath = "//span[text()='Presentation type']/..") %>%
      html_text() %>%
      na_if_empty(),
    # 'empty' on special panels (will be in `session_ref` instead)
    session_title = html_text(html_nodes(h, "header h1")) %>%
      na_if_empty(),
    # should be a single person
    chair = html_nodes(h, xpath = "//h3[text()='Chairs']/following::div[1]//span") %>%
      html_text() %>%
      na_if_empty(),
    chair_affiliation = html_nodes(h, xpath = "//h3[text()='Chairs']/following::div[1]//span/following::div[1]") %>%
      html_text() %>%
      na_if_empty(),
    # 1-2 persons
    discussant = html_nodes(h, xpath = "//h3[text()='Discussants']/following::div[1]//span") %>%
      html_text() %>%
      na_if_empty(),
    discussant_affiliation = html_nodes(h, xpath = "//h3[text()='Discussants']/following::div[1]//span/following::div[1]") %>%
      html_text() %>%
      na_if_empty()
  ) %>%
    bind_rows(d)

  # abstracts (0-6 rows, collecting only URL-based ids at that stage)
  a <- tibble::tibble(
    session_id = i,
    # numeric id in the abstract URL
    abstract_id = html_nodes(h, xpath = "//a[contains(@href, 'submission')]") %>%
      html_attr("href")#,
    # # internal numeric id
    # abstract_ref = html_nodes(h, xpath = "//a[contains(@href, 'submission')]/div[1]") %>%
    #   html_text(),
    # abstract_title = html_nodes(h, xpath = "//a[contains(@href, 'submission')]/div[2]") %>%
    #   html_text(),
    # abstract_* etc.
  ) %>%
    bind_rows(a)

}

# `full_join` in order to detect sessions that failed to parse
d <- full_join(d, a, by = "session_id", relationship = "many-to-many") %>%
  mutate(
    session_id = str_extract(session_id, "\\d+"),
    session_track = str_remove(session_track, "^Track"),
    session_type = str_remove(session_type, "^Presentation type"),
    session_title = if_else(is.na(session_title), session_ref, session_title),
    session_ref = if_else(session_ref == session_title, NA, session_ref),
    abstract_id = str_extract(abstract_id, "\\d+$"),
  ) %>%
  mutate_if(is.character, na_if, "")

# detect sessions that failed to parse (`d` in loop above)
stopifnot(!is.na(d$session_title))
# View(filter(d, is.na(session_title)))

# n = 275 unique sessions, 100% of downloaded pages
stopifnot(n_distinct(d$session_id) == length(f))

# session titles are NOT unique (single case)
distinct(d, session_id, session_title) %>%
  count(session_title) %>%
  filter(n > 1)

s <- distinct(select(d, -abstract_id))

# chairs and discussants are missing (for special, non-paper-based sessions),
# and some individuals chaired or discussed multiple times over the conference
count(s, chair, sort = TRUE) %>%
  filter(n > 1)
count(s, discussant, sort = TRUE) %>%
  filter(n > 1)

# missing affiliations for n = 96 chairs
filter(s, !is.na(chair), is.na(chair_affiliation)) %>%
  select(session_id, contains("chair")) %>%
  distinct()

# missing affiliations for n = 119 discussants
filter(s, !is.na(discussant), is.na(discussant_affiliation)) %>%
  select(session_id, contains("discussant")) %>%
  distinct()

# ... but that's fine, since we get affiliations from parsing the participants
#     pages in `04-participants` instead

# n = 0 session with multiple chairs
group_by(s, session_id) %>%
  filter(n_distinct(chair) > 1)

# n = 62 sessions with multiple discussants
group_by(s, session_id) %>%
  filter(n_distinct(discussant) > 1)

# n = 29 sessions where the chair is also a discussant
filter(s, chair == discussant) %>%
  select(session_id, chair, discussant)

# export ------------------------------------------------------------------

# uneconomical, multiple rows per session, some information repeats
readr::write_tsv(d, "data/sessions.tsv")

# wip
