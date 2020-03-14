
# Covid 19 modelling ------------------------------------------------------

# Packages & functions -------------------------------------------------------
library(readxl)
library(dplyr)
library(reshape2)
library(ggplot2)
library(rlang)
library(ranger)
library(forecast)
source("covid_functions.R")

# Download source data ----------------------------------------------------
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


# Get data for Italy ------------------------------------------------------
model_data_it <- get_model_data(
  df = eu_covid,
  some_country = "Italy"
)

it_split <- get_train_test_data(model_data_it, 0.8)

# Get data for Germany ----------------------------------------------------
model_data_de <- get_model_data(
  df = eu_covid,
  some_country = "Germany"
)

de_split <- get_train_test_data(model_data_de, 0.8)

# Get data for France -----------------------------------------------------
model_data_fr <- get_model_data(
  df = eu_covid,
  some_country = "France"
)

fr_split <- get_train_test_data(model_data_fr, 0.8)

# Modelling ---------------------------------------------------------------
linear_model_it <- lm(
  formula = confirmed ~ .,
  data = it_split$train_data
)

preds_it <- predict(
  linear_model_it, 
  newdata = it_split$test_data
)

# OOS Fit -----------------------------------------------------------------
mape(obs = it_split$test_data$confirmed, pred = preds_it)
mae(obs = it_split$test_data$confirmed, pred = preds_it)

# Compare observed with fit -----------------------------------------------
it_covid <- eu_covid %>%
  dplyr::filter(country == "Italy")

it_covid <- it_covid %>% 
  dplyr::filter(date_rep >= get_start_date(it_covid))


# Insample fit for Italy --------------------------------------------------
it_ts_plot <- it_covid %>%   
  mutate(Fit = preds_it) %>%
  dplyr::select(date_rep, confirmed, Fit) %>% 
  melt(id.vars = 'date_rep') %>% 
  ggplot(aes(x = date_rep, y = value, linetype = variable)) +
  geom_line() + 
  theme_minimal() +
  labs(
    x = "Date reported",
    y = "Confirmed cases",
    linetype = ""
  )

# Use the same model to predict German cases ------------------------------
preds_de <- predict(
  linear_model,
  newdata = model_data_de
)

# get German data to compare lines
de_covid <- eu_covid %>%
  dplyr::filter(country == "Germany")

de_covid <- de_covid %>% 
  dplyr::filter(date_rep >= get_start_date(de_covid))


# German ts plot ----------------------------------------------------------
de_covid %>%   
  mutate(Fit = preds_de) %>%
  dplyr::select(date_rep, confirmed, Fit) %>% 
  melt(id.vars = 'date_rep') %>% 
  ggplot(aes(x = date_rep, y = value, linetype = variable)) +
  geom_line() + 
  theme_minimal() +
  labs(
    x = "Date reported",
    y = "Confirmed cases",
    linetype = ""
  )

