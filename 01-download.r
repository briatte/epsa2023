library(RSelenium)
library(rvest)
library(tidyverse)

fs::dir_create("html")
fs::dir_create("html/authors") # all participants, really
fs::dir_create("html/abstracts")
fs::dir_create("html/sessions")

# all session ids (main programme page manually downloaded)
p <- read_html("html/Program • EPSA 2023.html") %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  str_subset("/session/\\d{5}") %>%
  str_replace(".*?#/", "https://virtual.oxfordabstracts.com/#/") %>%
  unique()

# initialize headless browser ---------------------------------------------

dr <- rsDriver(browser = "firefox",
               port = 4554L,
               verbose = FALSE,
               chromever = NULL)

rd <- dr[["client"]]

# display authors' list (returns information used in next section)
rd$navigate("https://virtual.oxfordabstracts.com/#/event/3738/people")
Sys.sleep(3)

# click time zone menu list
tz1 <- rd$findElement(using = "css selector", "button.flex")
tz1$clickElement()

# click first item (GMT+1)
tz2 <- rd$findElement(using = "css selector", "button.text-left")
tz2$clickElement()

# click 'Save'
tz3 <- rd$findElement(using = "css selector", "button.inline-flex")
tz3$clickElement()

# download authors --------------------------------------------------------

# find last page
n <- rd$getPageSource()[[1]] %>%
  read_html() %>%
  html_nodes(xpath = "//a[contains(@href, '?page=')]") %>%
  html_attr("href") %>%
  sort() %>%
  last() %>%
  str_extract("\\d+$")

# sanity check (since this sometimes returns NA instead of 95...)
stopifnot(!is.na(n))

cat("Downloading authors listing,", n, "pages\n")
for (i in as.integer(n):1) {

  f <- str_c("html/authors/authors_", i, ".html")
  if (!fs::file_exists(f)) {

    "https://virtual.oxfordabstracts.com/#/event/3738/people?page=" %>%
      str_c(., i) %>%
      rd$navigate()

    Sys.sleep(2) # very much required to avoid download empty lists

    rd$getPageSource()[[1]] %>%
      readr::write_lines(f)

    if (!i %% 10)
      cat(".")

  }

}

stop('auth')
# download sessions and abstracts -----------------------------------------

p <- sample(p)
for (i in rev(p)) {

  f <- str_c("html/sessions/session_", str_extract(i, "\\d+$"), ".html")
  if (!fs::file_exists(f)) {

    rd$navigate(i)
    Sys.sleep(2)

    rd$getPageSource()[[1]] %>%
      readr::write_lines(f)

  }

  h <- read_html(f)
  cat(which(p == i), fs::path_file(f), ":",
      html_text(html_nodes(h, "h1.mb-2")), # sometime empty (e.g. receptions)
      str_trunc(html_text(html_nodes(h, "h1.mt-4")), 40), "\n")

  # trivia: last 'session' is a golf event

  a <- html_nodes(h, "a") %>%
    html_attr("href") %>%
    str_subset("submission/\\d+") %>%
    str_replace(".*?#/", "https://virtual.oxfordabstracts.com/#/") %>%
    unique()

  if (!length(a))
    next

  # download abstracts, sometimes 0, sometimes 3, generally 4-5
  for (j in a) {

    f <- str_c("html/abstracts/abstract_", str_extract(j, "\\d+$"), ".html")
    if (!fs::file_exists(f)) {

      rd$navigate(j)
      Sys.sleep(2)

      rd$getPageSource()[[1]] %>%
        readr::write_lines(f)

    }

    h <- read_html(f)
    cat("  ", str_pad(fs::path_file(f), 18, side = "right"), ":",
        # never empty, but varies without matching session id?
        html_text(html_nodes(h, "h2.mb-1")),
        str_trunc(html_text(html_nodes(h, "h1.my-5")), 40), "\n")

  }

}

cat(
  length(fs::dir_ls("html/authors")), "author pages,",
  length(fs::dir_ls("html/sessions")), "sessions,",
  length(fs::dir_ls("html/abstracts")), "abstracts.\n"
)

# kthxbye
