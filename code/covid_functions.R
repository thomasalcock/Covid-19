

# Get source data ---------------------------------------------------------

get_source_data <- function(some_env = globalenv()){
  # Automate file download
  url <- "https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-2020-03-14.xls"
  
  file_name <- paste0("covid-data-", Sys.Date())
  
  download.file(url, destfile = file_name)
  
  assign(
    x = "df",  
    value = readxl::read_xls(file_name), 
    envir = some_env
  )
}

# Function to pick out start date according to reported confirmed  --------
get_start_date <- function(df, var_name = 'confirmed'){
  
  df %>% 
    dplyr::group_by(date_rep) %>% 
    dplyr::filter(!!sym(var_name) > 0) %>% 
    dplyr::ungroup() %>% 
    dplyr::arrange(date_rep) %>% 
    dplyr::slice(1) %>%
    dplyr::pull(date_rep)
  
}

# Function to produce model data ------------------------------------------
generate_features <- function(df, some_country){
  
  # generate features
  df <- df %>% 
    dplyr::filter(grepl(some_country, country)) %>% 
    dplyr::mutate(
      time_rep = time(date_rep),
      time_rep_sq = time_rep^2,
      time_rep_cub = time_rep^3,
      time_rep_qu = time_rep^4,
      lag_1 = dplyr::lag(confirmed),
      lag_2 = dplyr::lag(confirmed, 2)
    ) %>% 
    na.omit()
  
  # get start date
  start_date <- get_start_date(
    df = df, 
    var_name = "confirmed"
  )
  
  # filter by start date
  df <- df %>%
    filter(date_rep >= start_date)
  
  # select columns
  df <- df %>% 
    dplyr::select(
      confirmed, 
      starts_with("time"),
      starts_with("lag")
    )
  
  return(df)
}


# function to create train test split -------------------------------------
get_train_test_data <- function(df, frac){
  
  train_data <- df %>% 
    dplyr::slice(1:round(nrow(.) * frac))
  
  test_data <- df %>% 
    dplyr::slice((round(nrow(.) * frac) + 1):nrow(.))
  
  return(list(train_data = train_data, test_data = test_data))
  
}


# Dataprep wrapper --------------------------------------------------------
generate_model_data <- function(country, split_frac){
  
  get_source_data()
  
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
  
  df <- df %>% 
    dplyr::filter(region == "EU") %>% 
    dplyr::group_by(date_rep, country) %>% 
    dplyr::summarise(
      confirmed = sum(confirmed, na.rm = TRUE),
      dead = sum(dead, na.rm = TRUE)
    ) %>% 
    dplyr::ungroup()
  
  # save intermediary data in global 
  # environment
  c_assign(df, "raw_data")
  
  # create features
  model_data <- generate_features(
    df = df,
    some_country = country
  )
  
  # train test split
  country_split <- get_train_test_data(
    df = model_data, 
    frac = split_frac
  )
  
  # assign train data to global env
  c_assign(
    obj = country_split$train_data,
    name = "train_data"
  )
  
  # assign test data to global env
  c_assign(
    obj = country_split$test_data,
    name = "test_data"
  )
  
}


# Assignment method for wrapper function ----------------------------------
c_assign <- function(obj, name){
  assign(
    x = name,
    value = obj,
    envir = globalenv()
  )
}

# Prediction error functions ----------------------------------------------
mape <- function(obs, pred){
  100 * mean(abs((obs - pred)/obs))
}

mae <- function(obs, pred){
  mean(abs(obs - pred))
}


# Utils -------------------------------------------------------------------
ifna <- function(x){
  ifelse(is.na(x) | is.nan(x) |is.infinite(x), 0, x)
}
