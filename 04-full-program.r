library(tidyverse)
library(rvest)

# [NOTE] code below adapted from that used in `briatte/epsa2020`, with major
#        differences (different code on chairs/discussants, more data cleaning)

# [NOTE] well, code copied from `epsa2021`, with minimal edits, keeping even
#        some fixes that should be idiosyncratic to 2021...

# TODO: fix TODO notes, and inspect entire thing much more thoroughly

# [NOTE] all program information extracted from abstracts
#        (we downloaded session pages but do not use them here)

f <- fs::dir_ls("html/abstracts", glob = "*.html")

d <- map(f, read_html) %>%
  map_dfr(
    ~ tibble::tibble(
      # panels
      session_id = html_node(.x, "meta[name='session_id']") %>%
        html_attr("content"),
      # strictly identical to meta name = 'session_full_title'
      session_title = html_node(.x, "meta[name='full_title']") %>%
        html_attr("content"),
      # actually contains either 'Panel' or, in one case, 'Book Panel'
      session_type = html_node(.x, "meta[name='session_short_title']") %>%
        html_attr("content"),
      # contains 'Panel Session 1', 'Panel Session 2', etc.
      # session_type = html_node(.x, "meta[name='session_type']") %>%
       # html_attr("content"),
      # participants
      # [NOTE] missing affiliations for chairs...
      # [NOTE] missing affiliations for discussants...
      chairs = html_node(.x, "meta[name='chairs']") %>%
        html_attr("content"),
      # above is equivalent to:
      # chairs = html_node(.x, xpath = "//div[starts-with(text(), 'Chair/s')]/..") %>%
      #   html_text(),
      # meta name = "moderators" is always empty, so...
      discussants = html_node(.x, xpath = "//div[starts-with(text(), 'Discussant/s')]/..") %>%
        html_text(trim = TRUE),
      authors = html_node(.x, "meta[name='authors']") %>%
        html_attr("content"),
      affiliations = html_node(.x, "meta[name='affiliations']") %>%
        html_attr("content"),
      # abstracts
      abstract_title = html_node(.x, "meta[name='abstracttitle']") %>%
        html_attr("content"),
      # [NOTE] actual `paper_ref` meta tag contains identifiers that repeat
      # over panels, and so are useless to uniquely identify papers
      abstract_ref = html_node(.x, "meta[name='ID']") %>%
        html_attr("content"),
      # always empty
      # paper_type = html_node(.x, "meta[name='pres_type']") %>%
      #   html_attr("content"),
      # [NOTE] unlike `epsa2020`, correctly identifies multiple presenters
      abstract_presenters = html_node(.x, "meta[name='presenters']") %>%
        html_attr("content"),
      abstract_text = html_node(.x, ".abstracttext") %>%
        html_text(trim = TRUE),
      # [NOTE] topics used here are panel track names, but are used as keywords
      #        on abstracts, __and are not stable within sessions__
      abstract_topic = html_node(.x, "meta[name='topic']") %>%
        html_attr("content")
    ),
    .id = "abstract_id"
  ) %>%
  mutate(
    # authors have extra spaces but are clean otherwise
    authors = str_squish(authors) %>%
      # minimal data cleaning
      str_remove_all("(Prof|Dr)\\.\\s") %>%
      # single fix that helps a lot with (chair) affiliations
      str_replace("Andreas Goldberg", "Andreas C Goldberg"),
    chairs = str_squish(chairs) %>%
      # sometimes missing entirely
      na_if(""),
    discussants = str_squish(discussants),
    # abstract presenters are always 100% clean, except for this below, once
    abstract_presenters = abstract_presenters %>%
      # single fix that helps a lot with (chair) affiliations
      # [NOTE] this fix is from 2021, but let's keep it here too for now?
      # TODO; check whether this makes any sense
      str_replace("Andreas Goldberg", "Andreas C Goldberg"),
    # same goes for affiliations and abstracts, where there are just a few \r\n
    affiliations = str_squish(affiliations) %>%
      # minimal data cleaning
      # [1] affiliations that start with "PhD candidate/researcher [in…]"
      # str_subset(d$affiliations, "PhD.*?,\\s")
      str_remove_all("PhD.*?,\\s") %>%
      # [2] affiliations that start with "Assoc./Assist. Prof."
      # str_subset(d$affiliations, "(Associate|Assistant)\\sProfessor,\\s")
      str_remove_all("(Associate|Assistant)\\sProfessor,\\s"),
    # clean up prefixes
    chairs = str_remove(chairs, "^Chair/s:"),
    discussants = str_remove(discussants, "^Discussant/s:\\s"),
    abstract_text = str_squish(abstract_text),
    # `abstract_id` is 4-padded
    abstract_id = str_remove_all(basename(abstract_id), "\\D")
  )

# sanity checks
stopifnot(!is.na(d$session_id))
stopifnot(!duplicated(d$abstract_id))
stopifnot(!duplicated(d$abstract_ref))

# match authors and affiliations ------------------------------------------

# problem: (bracketed part) in names will cause issues,
# e.g. 'Ameetosri (Amy) Basu'
str_subset(d$authors, "Basu")

# # problem: commas in one particular form of affiliation numbers will create
# issues in the next code block, so remove it first
# str_extract_all(d$authors, "\\(.*?\\)") %>%
#   unlist() %>%
#   table()

# # remove spaces from affiliation numbers
# str_replace_all(d$authors, ",\\s(\\d)\\)", ",\\1)") %>%
#   str_extract_all("\\(.*?\\)") %>%
#   unlist() %>%
#   table()

d$authors <- str_replace_all(d$authors, ",\\s(\\d)\\)", ",\\1)") %>%
  map(
    # split on comma-space (never found in names or affiliation numbers)
    ~ str_split(.x, ",\\s") %>%
      unlist() %>%
      as_tibble_col(column_name = "author") %>%
      mutate(
        aid = str_extract(author, "\\(.*?\\)") %>%
          str_split(",") %>%
          map(unlist) %>%
          map(str_remove_all, "\\D") %>%
          map(as.integer)
      ),
  ) %>%
  map(~ unnest(.x, aid)) %>%
  map(~ mutate(.x, aid = if_else(is.na(aid), 1L, aid)))

d$affiliations <- d$affiliations %>%
  map(
    ~ str_split(.x, "\\d\\.\\s?") %>%
      unlist() %>%
      str_subset("\\w{1,}") %>%
      as_tibble_col(column_name = "affiliation") %>%
      # re-add numbers (affiliations are always in numeric order)
      add_column(aid = 1:nrow(.), .before = 1)
  )

d$matched <- map2(d$authors, d$affiliations, ~ full_join(.x, .y, by = "aid"))

d <- tidyr::unnest(d, matched) %>%
  select(-authors, -affiliations, -aid) %>%
  # lose 8 cases of lone affiliations that weren't matched to an author
  # TODO: re-examine whether anything might help fix that
  filter(!is.na(author)) %>%
  # clean some punctuation
  mutate(
    author = str_remove(author, "\\s\\(.*?\\)"),
    affiliation = str_remove(affiliation, ",\\s?$") %>%
      str_squish()
  ) %>%
  # collapse multiple affiliations
  group_by(abstract_id, author) %>%
  mutate(affiliation = str_flatten(affiliation, collapse = " && ")) %>%
  # remove duplicated rows
  distinct() %>%
  ungroup()

# n = 57 authors with multiple affiliations
sum(str_detect(d$affiliation, "&&"))

# sanity check: all abstract presenters are present in abstract authors
y <- unique(d$abstract_presenters) %>%
  str_split(",\\s") %>%
  unlist() %>%
  unique() %>%
  str_remove("\\s\\(.*?\\)") # fixes "Ameetosri (Amy) Basu"

stopifnot(y %in% d$author)

# check whether chairs exist as authors -----------------------------------

# chairs
y <- str_split(d$chairs, ",\\s") %>%
  unlist() %>%
  unique() %>%
  sort()

# n = 51 cases for which affiliations cannot be retrieved from authors
# n = 147 cases where that's possible
table(y %in% unique(d$author))

# check whether discussants exist as authors ------------------------------

# discussants
z <- str_split(d$discussants, ",\\s") %>%
  unlist() %>%
  unique() %>%
  sort()

# n = 39 cases for which affiliations cannot be retrieved from authors
# n = 171 cases where that's possible
table(z %in% unique(d$author))

# authors with multiple affiliations --------------------------------------

# n = 241 (114 cases with 2-3 affiliations each), fixable via manual fixes file
p <- d %>%
  distinct(author, affiliation) %>%
  arrange(author) %>%
  group_by(author) %>%
  mutate(n_affiliations = n_distinct(affiliation)) %>%
  filter(n_affiliations > 1)

# n_distinct(p$author)
# table(p$n_affiliations)

# start fixes here, as it will otherwise create issues with the code below when
# we impute affiliations from authors:
#
# some chairs have multiple affil. as authors
# table(y %in% p$author)
# some discussants have multiple affil. as authors
# table(z %in% p$author)

# fix affiliations for authors with multiple ones -------------------------

# initialize affiliation problems file for authors with multiple affiliations
# AND for chairs and discussants without an affiliation found among authors,
# which we fix later after reformatting the data
bind_rows(
  # authors with multiple affiliations
  select(p, full_name = author, affiliation),
  # chairs with no affiliations found in authors
  tibble::tibble(
    full_name = y[ !y %in% unique(d$author) ],
    affiliation = NA_character_
  ),
  # discussants with no affiliations found in authors
  tibble::tibble(
    full_name = z[ !z %in% unique(d$author) ],
    affiliation = NA_character_
  )
) %>%
  distinct() %>%
  arrange(full_name, affiliation) %>%
  readr::write_tsv("data/affiliation-problems.tsv")

# create `affiliation-fixes.tsv` in Google Sheets and export to `data/` before
# moving on to the next steps

# sanity checks
y <- readr::read_tsv("data/affiliation-problems.tsv", col_types = "cc")
z <- readr::read_tsv("data/affiliation-fixes.tsv", col_types = "ccc")
# - fixes cover all initially problematic cases
stopifnot(y$full_name %in% z$full_name)
# - fixes contain nothing but initially problematic cases
stopifnot(z$full_name %in% y$full_name)
# - no weird or missing values in fixes
stopifnot(!is.na(y$full_name) & y$full_name != "")
# - no duplicate names in fixes
stopifnot(!duplicated(z$full_name))

# `z` contains affiliation AND a few full name fixes
# use it to get a single affiliation per author
d <- left_join(d, z, by = c("author" = "full_name")) %>%
  mutate(
    author = if_else(is.na(full_name_fix), author, full_name_fix),
    affiliation = if_else(is.na(affiliation.y), affiliation.x, affiliation.y)
    ) %>%
  select(-full_name_fix, -affiliation.x, -affiliation.y) %>%
  relocate(affiliation, .after = "author")

# sanity checks
stopifnot(!is.na(d$author))
stopifnot(!is.na(d$affiliation)) # applies to authors/presenters only here

# reformat ----------------------------------------------------------------

d <- bind_rows(
  # chairs (n = 229 after unnesting panels with multiple chairs)
  select(d, starts_with("session_"), full_name = chairs) %>%
    # discard rows with no chairs
    filter(!is.na(full_name)) %>%
    distinct() %>%
    # handle the 1 case with two chairs
    # filter(str_detect(full_name, ","))
    mutate(full_name = str_split(full_name, ",\\s")) %>%
    tidyr::unnest(full_name) %>%
    # get affiliations from authors rows when possible, `NA` otherwise
    left_join(distinct(d, author, affiliation), by = c("full_name" = "author")) %>%
    add_column(role = "c"),
  # discussants (n = 136 after unnesting panels with multiple discussants)
  select(d, starts_with("session_"), full_name = discussants) %>%
    # discard rows with no discussants
    filter(!is.na(full_name)) %>%
    distinct() %>%
    # handle the 10 cases with two chairs
    # filter(str_detect(full_name, ","))
    mutate(full_name = str_split(full_name, ",\\s")) %>%
    tidyr::unnest(full_name) %>%
    # get affiliations from authors rows when possible, `NA` otherwise
    left_join(distinct(d, author, affiliation), by = c("full_name" = "author")) %>%
    add_column(role = "d"),
  # authors
  select(d, -chairs, -discussants, full_name = author) %>%
    add_column(role = "p")
) %>%
  arrange(session_id, role, full_name)

# sanity checks
stopifnot(!is.na(d$full_name))
stopifnot(d$full_name != "Shared by Panellists") # see `epsa2021` for context

# fix affiliations for chairs and discussants -----------------------------

# apply name and affiliation fixes again (some were for chairs/discussants)
d <- left_join(d, z, by = "full_name") %>%
  mutate(
    full_name = if_else(is.na(full_name_fix), full_name, full_name_fix),
    affiliation = if_else(is.na(affiliation.y), affiliation.x, affiliation.y)
  ) %>%
  select(-full_name_fix, -affiliation.x, -affiliation.y) %>%
  relocate(affiliation, .after = "full_name")

# sanity checks
stopifnot(!is.na(d$full_name))
stopifnot(!is.na(d$affiliation))

# create unique participant identifiers -----------------------------------

# [NOTE] should be reproducible using R >= 3.5.0, serialization version 3
#        see ?rlang::hash -- 128-bit hashes

p <- distinct(d, full_name, affiliation) %>%
  add_column(conference = "epsa2022", .before = 1) %>%
  mutate(
    # `affiliation` is actually never missing here, so `str_replace_na` is
    # used only as a precaution here (an actually useful one until we fix
    # missing affiliations... see TODO note above)
    text = str_c(conference, full_name, str_replace_na(affiliation)),
    # create 32-length UIDs
    hash = map_chr(text, rlang::hash)
  )

# sanity checks: no duplicates
stopifnot(!duplicated(p$text))
stopifnot(!duplicated(p$hash))

# add hashes to master data
d <- select(p, full_name, affiliation, pid = hash) %>%
  left_join(d, ., by = c("full_name", "affiliation"), relationship = "many-to-one") %>%
  relocate(pid, .before = "full_name")

# sanity check: no missing pid
stopifnot(!is.na(d$pid))

# export ------------------------------------------------------------------

stopifnot(!duplicated(d))

cat(
  n_distinct(d$session_id), "sessions,",
  n_distinct(d$abstract_id), "abstracts,",
  n_distinct(d$pid), "unique participant IDs,",
  n_distinct(d$full_name), "unique names.\n"
)

readr::write_tsv(d, "data/program.tsv")

# that was pretty epic
# kthxbye
