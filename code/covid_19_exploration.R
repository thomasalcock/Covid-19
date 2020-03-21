

# Covid 19 Exploration ----------------------------------------------------

# Packages and data -------------------------------------------------------
library(readxl)
library(dplyr)
library(reshape2)
library(ggplot2)
library(rlang)
library(glmnet)
source("code/covid_functions.R")

# download source data ----------------------------------------------------
get_source_data()


# Rename columns ----------------------------------------------------------
df <- df %>% 
  dplyr::rename(
    date_rep = DateRep,
    country = CountryExp,
    confirmed = NewConfCases,
    dead = NewDeaths,
    geo_id = GeoId,
    gaul1_nuts1 = Gaul1Nuts1,
    region = EU
  )


# Confirmed EU cases over time by country ---------------------------------
eu_covid <- df %>% 
  dplyr::filter(region == "EU") %>% 
  dplyr::group_by(date_rep, country) %>% 
  dplyr::summarise(
    confirmed = sum(confirmed, na.rm = TRUE),
    dead = sum(dead, na.rm = TRUE)
  ) %>% 
  dplyr::ungroup()


# choosing relevant countries and dates -----------------------------------

# let's look only at countries which have had people die at all.
countries_of_interest <- eu_covid %>% 
  dplyr::group_by(country) %>% 
  dplyr::filter(sum(dead) > 0) %>%
  dplyr::ungroup() %>% 
  dplyr::pull(country) %>% 
  unique()

start_date_confirmed <- get_start_date(df = eu_covid)

# TS plot of confirmed by EU country ----------------------------------------
eu_covid %>% 
  dplyr::filter(
    country %in% c("Germany", "Italy", "Spain") & 
    date_rep >= start_date_confirmed
  ) %>% 
  ggplot(aes(x = date_rep, y = confirmed)) +
  geom_line() +
  theme_minimal() +
  labs(
    x = "Date reported", 
    y = "Confirmed"
  ) +
  facet_wrap(
    facets = ~country, 
    nrow = 3, 
    ncol = 1
  )

