
# Covid_19 Modeling -------------------------------------------------------

source("code/covid_functions.R")

generate_model_data(
  country = "Italy",
  split = 0.9
)

# Modelling ---------------------------------------------------------------
linear_model_it <- lm(
  formula = confirmed ~ .,
  data = train_data
)

preds_it <- predict(
  linear_model_it, 
  newdata = test_data
)

# OOS Fit -----------------------------------------------------------------
mape_it <- mape(
  obs = test_data$confirmed,
  pred = preds_it
)