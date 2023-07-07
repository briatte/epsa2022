library(tidyverse)
library(rvest)

fs::dir_create("data")

d <- tibble::tibble()
for (i in fs::dir_ls("html/sessions/", regexp = "session")) {

  h <- read_html(i)

  d <- tibble::tibble(
    session = i,
    title = html_node(h, "div[class='session_link']") %>%
      html_text(),
    chairs = html_node(h, xpath = "//div[contains(text(), 'Chair/s')]/following::div") %>%
      html_text(),
    discussants = html_node(h, xpath = "//div[contains(text(), 'Discussant/s')]/following::div") %>%
      html_text(),
    abstract = html_nodes(h, xpath = "//a[contains(@href, '/abstract')]") %>%
      html_attr("href")
  ) %>%
    bind_rows(d)

}

d <- d %>%
  mutate(
    session = fs::path_file(session),
    chairs = str_squish(chairs),
    discussants = str_squish(discussants),
    abstract = fs::path_file(abstract)
  )

View(d)

# n = 228 unique sessions
s <- distinct(select(d, -abstract))

# session titles are NOT unique
s$title[ duplicated(s$title) ]

# chairs and discussants are missing for special (non-paper-based) panels,
# and some individuals chaired or discussed multiple times; there is also a
# special case of 'Shared by Panellists' in `chairs` and `discussants`
count(s, chairs, sort = TRUE)
count(s, discussants, sort = TRUE)

# only n = 1 case of multiple chairs
s$chairs[ str_detect(s$chairs, ",") ]

# n = 5 cases of multiple discussants (beyond 'Shared by Panellists' cases)
s$discussants[ str_detect(s$discussants, ",") ]

# there is no affiliation for chairs or discussants: find them from abstracts,
# if they also authored a paper?

# uneconomical export
readr::write_tsv(d, "data/sessions.tsv")

# wip
