library(RSelenium)
library(rvest)
library(tidyverse)

fs::dir_create("html")
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

# display random page
i <- sample(p, 1)
rd$navigate(i)
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
  length(fs::dir_ls("html/sessions", glob = "*.html")), "sessions,",
  length(fs::dir_ls("html/abstracts", glob = "*.html")), "abstracts.\n"
)

# kthxbye
