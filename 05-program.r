# adapted from an earlier year, but we do things differently regarding chairs
# and discussants, given the extra work in the previous script to extract them
# with 'unified' affiliations

library(tidyverse)

p <- readr::read_tsv("data/participants.tsv", col_types = "ccccc")

d <- readr::read_tsv("data/sessions.tsv", col_types = "ccccccccc") %>%
  full_join(select(filter(p, role == "p"), author, affiliation, abstract_id),
            by = "abstract_id")

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

# sanity check: no missing values in 'authors' (participants, really)
stopifnot(!is.na(p$author))

# NOTE -- no need for `distinct` below, c/d roles do not repeat within sessions
p <- bind_rows(
  # chairs
  filter(p, role == "c") %>%
    select(session_id, full_name = author, affiliation, role) %>%
    arrange(full_name, session_id)
  ,
  # discussants
  filter(p, role == "d") %>%
    select(session_id, full_name = author, affiliation, role) %>%
    arrange(full_name, session_id)
  ,
  # authors (presenters)
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
