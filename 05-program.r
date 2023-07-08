library(tidyverse)

d <- read_tsv("data/sessions.tsv", col_types = "ccccccccc") %>%
  full_join(read_tsv("data/authors.tsv", col_types = "ccc"), by = "abstract_id")

# sanity checks: session identifiers are never missing
stopifnot(!is.na(d$session_id))

# a few sessions are receptions and similar events with no abstracts
filter(d, is.na(abstract_id)) %>%
  select(session_id, session_title)

# keep only sessions with abstracts
d <- filter(d, !is.na(abstract_id))
stopifnot(!is.na(d$abstract_id))

# a few special sessions have abstracts but not ref.
filter(d, is.na(session_ref)) %>%
  select(session_id, session_title) %>%
  distinct()

# [NOTE] chairs and discussants are sometimes, but not too often, missing
n_distinct(d$session_id[ is.na(d$chair) ])
n_distinct(d$session_id[ is.na(d$discussant) ])

# assemble participants ---------------------------------------------------

# participants
p <- bind_rows(
  # chairs
  select(d, session_id, starts_with("chair")) %>%
    distinct() %>%
    rename(full_name = chair, affiliation = chair_affiliation) %>%
    filter(!is.na(full_name)) %>%
    add_column(role = "c")
  ,
  # discussants
  select(d, session_id, starts_with("discussant")) %>%
    distinct() %>%
    rename(full_name = discussant, affiliation = discussant_affiliation) %>%
    filter(!is.na(full_name)) %>%
    add_column(role = "d")
  ,
  # authors
  select(d, session_id, full_name = author, affiliation, abstract_id) %>%
    # next lines not required: authors do not repeat and are never missing
    # distinct() %>%
    # filter(is.na(full_name))
    add_column(role = "p")
)

# reduce sessions ---------------------------------------------------------

# sessions, keeping only actual session columns
d <- select(d, starts_with("session")) %>%
  distinct()

# assemble full programme -------------------------------------------------

# add abstracts
a <- read_tsv("data/abstracts.tsv", col_types = "ccccc")

# `full_join` because `d` and `p` have exactly the same list of `session_id`
d <- full_join(d, p, by = "session_id") %>%
  # `left_join` because `d` has rows where `abstract_id` is `NA` (chairs, disc.)
  left_join(a, by = "abstract_id")

# sanity check: everything belongs in a session
stopifnot(!is.na(d$session_id))

# export ------------------------------------------------------------------

d <- arrange(d, session_id, role, abstract_id)

# uneconomical export (lots of repeated information)
readr::write_tsv(d, "data/program.tsv")

# kthxbye
