
# Exploring COVID-19 Data -------------------------------------------------

pacman::p_load(tidyverse, httr, magrittr)

url_base <- "https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_daily_reports"
url_api <- "https://api.github.com/repos/CSSEGISandData/COVID-19/git/trees/master?recursive=1"
url_raw <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/"

request <- GET(url_api)
stop_for_status(request)

filelist_gh <-
  content(request) %>%
  pluck("tree") %>%
  map("path") %>%
  flatten_chr() %>%
  str_subset("csse_covid_19_data/csse_covid_19_daily_reports") %>%
  str_subset(".csv")

filelist_local <- dir("data", pattern = ".csv")

filelist_df <-
  tibble(gh = str_c(url_raw, filelist_gh)) %>%
  mutate(filename = word(gh, -1, sep = "/"),
         exists_locally = filename %in% filelist_local)

filelist_new <-
  filelist_df %>%
  filter(!exists_locally)

if (nrow(filelist_new) > 0) {
  filelist_new %$%
    walk2(gh, file.path("data", filename), download.file, quiet = TRUE)
}
