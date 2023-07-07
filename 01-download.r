library(tidyverse)
library(rvest)
library(httr)

fs::dir_create("html/conference-days")
fs::dir_create("html/sessions")
fs::dir_create("html/abstracts")

# all 3 conference days
for (i in str_c("https://coms.events/epsa-2022/en/day_", 1:3, ".html")) {

  f <- fs::path("html", "conference-days", fs::path_file(i))
  cat(f, ": ")

  if (!fs::file_exists(f)) {
    download.file(i, f, mode = "wb", quiet = TRUE)
  }

  s <- read_html(f) %>%
    html_nodes(xpath = "//a[contains(@href, 'session_')]") %>%
    html_attr("href") %>%
    str_replace("^\\.\\.", "https://coms.events/epsa-2022")

  cat(length(s), "sessions\n")

  for (j in s) {

    f <- fs::path("html", "sessions", fs::path_file(j))
    cat(f, ": ")

    if (!fs::file_exists(f)) {
      download.file(j, f, mode = "wb", quiet = TRUE)
    }

    a <- read_html(f) %>%
      html_nodes(xpath = "//a[contains(@href, 'abstract_')]") %>%
      html_attr("href") %>%
      str_replace("^\\.\\./\\.\\.", "https://coms.events/epsa-2022/data")

    cat(length(a), "abstracts")

    for (k in a) {

      f <- fs::path("html", "abstracts", fs::path_file(k))
      if (!fs::file_exists(f)) {
        download.file(k, f, mode = "wb", quiet = TRUE)
      }
      cat(".")

    }

    cat("\n")

  }

}

cat(
  length(fs::dir_ls("html/sessions", glob = "*.html")), "sessions,",
  length(fs::dir_ls("html/abstracts", glob = "*.html")), "abstracts.\n"
)

# kthxbye
