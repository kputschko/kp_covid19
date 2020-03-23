
# Exploring COVID19 -------------------------------------------------------

pacman::p_load(tidyverse, lubridate)

# Load Data ---------------------------------------------------------------

data_gh <-
  list(confirmed = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv",
       deaths    = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv",
       recovered = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv") %>%
  modify(read_csv, col_types = cols())

data_long <-
  data_gh %>%
  modify(pivot_longer, cols = `1/22/20`:last_col(), names_to = "date", values_to = "count") %>%
  bind_rows(.id = "status") %>%
  mutate_at("date", mdy) %>%
  rename(province_state = `Province/State`,
         country_region = `Country/Region`)


# New Cases ---------------------------------------------------------------

data_new <-
  data_long %>%
  filter(status == "confirmed", country_region %in% c("US", "Italy")) %>%
  group_by(country_region, date) %>%
  summarise(cases = sum(count)) %>%
  mutate(new_cases = cases - lag(cases),
         new_pct   = new_cases / lag(new_cases)) %>%
  filter(cases >= 100) %>%
  mutate(day_count = sequence(n())) %>%
  print()


# Plot --------------------------------------------------------------------

data_new %>%
  ggplot(aes(x = day_count, y = cases, color = country_region)) +
  geom_point()

data_new %>%
  ggplot(aes(x = date, y = cases, color = new_pct)) +
  geom_point() +
  scale_color_viridis_c(option = "B", direction = -1)

data_new %>%
  ggplot(aes(x = date, y = cases, color = country_region, size = new_pct)) +
  geom_point()

