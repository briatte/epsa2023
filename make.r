
# steps below largely modeled on `epsa2019/make.r`

# downloads ---------------------------------------------------------------

# note -- uses RSelenium, which rarely works perfectly out-of-the-box, so
# make sure to monitor this when it runs

source("01-download.r")

# parsing -----------------------------------------------------------------

source("02-sessions.r")
source("03-abstracts.r")
source("04-participants.r")

# wrangling ---------------------------------------------------------------

# reformat data into single master dataset (`program.tsv`)
source("05-program.r")

# [NOTE] if this below happens, it needs to happen before generating pids
#
# # fix multiple affiliations per author + minimal data cleaning
# source("07-fix-affiliations.r")

# create unique participant ids
source("06-pids.r")

# conclude ----------------------------------------------------------------

d <- readr::read_tsv("data/program.tsv", col_types = "ccccccccccccc")

cat(
  "\n-", n_distinct(d$session_id), "panels (with papers)",
  "\n-", n_distinct(d$abstract_id), "abstracts",
  "\n-", n_distinct(d$pid), "participants (pids)",
  "\n-", n_distinct(d$full_name), "participants (full names)",
  # TODO: more data cleaning on affiliations
  "\n-", n_distinct(d$affiliation), "affiliations\n"
)

# work-in-progress
