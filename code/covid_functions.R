

# Get source data ---------------------------------------------------------

get_source_data <- function(){
  # Automate file download
  url <- "https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-2020-03-14.xls"
  file_name <- paste0("covid-data-", Sys.Date())
  download.file(url, destfile = file_name)
  assign("df",  readxl::read_xls(file_name), envir = globalenv())
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
get_model_data <- function(df, some_country){
  
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

mape <- function(obs, pred){
  100 * mean(abs((obs - pred)/obs))
}

mae <- function(obs, pred){
  mean(abs(obs - pred))
}
