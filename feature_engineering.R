library(lightgbm)
library(tidyverse)
library(tsibble)
library(fable)
library(lubridate)
library(data.table)
library(dplyr)
library(Metrics)
library(zoo)

add_rolling_mean_sales <- function(dt, window = 14){
  
  dt <- dt %>%
    arrange(date, item_nbr, store_nbr) %>% # Sort by item_nbr, store_nbr and date
    group_by(item_nbr, store_nbr) %>% # Group by item_id
    mutate(rolling_avg_sales = rollapply(unit_sales, width = window, FUN = mean, fill = 0, align = "right")) %>%
    ungroup() %>% as.data.table()
 
  col_name = paste0("rolling_avg_sales_", window)
  
  dt[,col_name] <- dt$rolling_avg_sales
  dt <- dt %>% select(-rolling_avg_sales) # Drop the old column
  
  return(dt)
  
}






add_last_rolling_means <- function(train, test, window = 14) {
  
  # make sure we are in order (most recent date last)
  train <- train %>% arrange(date)
  
  # set the dynamic column name
  rolling_avg_col_name <- paste0("rolling_avg_sales_", window)
  
  # if we already have this column, remove it
  if(rolling_avg_col_name %in% names(test)){
    test <- test %>% select(-!!sym(rolling_avg_col_name))
  }
  
  # get the last rolling average sales for each item and store
  last_rolling_avg_sales <- train %>%
    group_by(item_nbr, store_nbr) %>%
    slice_tail(n = 1) %>%
    select(item_nbr, store_nbr, !!rolling_avg_col_name)
 
  # join the last rolling average sales to the test data and fill NAs with 0
  test <- test %>%
    left_join(last_rolling_avg_sales, by = c("item_nbr", "store_nbr"))
   
  test <- test %>%
    mutate(
      new_col = ifelse(is.na(!!sym(rolling_avg_col_name)), 0, !!sym(rolling_avg_col_name))
    ) %>%
    select(-!!sym(rolling_avg_col_name)) %>%
    rename(!!rolling_avg_col_name := new_col)
  
  
  # return the modified test data
  return(test)
}


add_last_rolling_sums <- function(train, test, window = 14) {
  
  # make sure we are in order (most recent date last)
  train <- train %>% arrange(date)
  
  # set the dynamic column name
  rolling_sum_col_name <- paste0("rolling_sum_promo_", window)
  
  # if we already have this column, remove it
  if(rolling_sum_col_name %in% names(test)){
    test <- test %>% select(-!!sym(rolling_sum_col_name))
  }
  
  # get the last rolling average sales for each item and store
  last_rolling_sum_promo <- train %>%
    group_by(item_nbr, store_nbr) %>%
    slice_tail(n = 1) %>%
    select(item_nbr, store_nbr, !!rolling_sum_col_name)
  
  # join the last rolling average sales to the test data and fill NAs with 0
  test <- test %>%
    left_join(last_rolling_sum_promo, by = c("item_nbr", "store_nbr"))
  
  test <- test %>%
    mutate(
      new_col = ifelse(is.na(!!sym(rolling_sum_col_name)), 0, !!sym(rolling_sum_col_name))
    ) %>%
    select(-!!sym(rolling_sum_col_name)) %>%
    rename(!!rolling_sum_col_name := new_col)
  
  
  # return the modified test data
  return(test)
}






add_day_of_week <- function(dt){
  dt$day_of_week <- lubridate::wday(dt$date, week_start = 1)
  return(dt)
}

add_is_weekend <- function(dt){
  dt$is_weekend <- as.integer(lubridate::wday(dt$date, week_start = 1) %in% c(6,7))
  return(dt)
}

add_is_weekend_workday <- function(dt){
  dt$is_weekend_workday <- ifelse(dt$type == "Work Day", 1, 0)
  return(dt)
}

get_mean_sales <- function(dt, group_cols, mean_col_name){
  avg_sales <- dt %>%
    group_by(across(all_of(group_cols))) %>%
    summarize(!!mean_col_name := mean(unit_sales))
  return(avg_sales)
}

add_mean_sales <- function(dt, means, join_cols, mean_col_name){
  dt <- dt %>%
    left_join(means, by=all_of(join_cols))
    #mutate(!!mean_col_name := coalesce(ifelse(is.na(as.numeric(!!mean_col_name)), 0, as.numeric(!!mean_col_name)), 0))
  return(dt)
}

get_promotion_percentage_diff <- function(dt) {
  result <- dt %>%
    group_by(item_nbr, onpromotion) %>%
    summarize(mean_unit_sales = mean(unit_sales)) %>%
    pivot_wider(names_from = onpromotion, values_from = mean_unit_sales) %>%
    mutate(promotion_percentage_diff = (1 - `0`/`1`) * 100)
  return(result)
}











add_is_national_holiday <- function(dt){
  dt$is_national_holiday <- 0
  dt$is_national_holiday <- ifelse(dt$locale == "National" & dt$transferred == 0, 1, dt$is_national_holiday)
  return(dt)
}

add_days_to_xmas_last30 <- function(dt){
  
  dt$days_to_xmas <- as.integer(difftime(as.Date(paste0(lubridate::year(dt$date), "-12-25")), dt$date, units = "days"))
  
  dt$days_to_xmas <- pmin(dt$days_to_xmas, 30)
  
  return(dt)
  
}

add_days_from_start_of_month_first14 <- function(dt){
  
  dt$days_from_start_of_month <- day(dt$date) - 1
  dt$days_from_start_of_month <- ifelse(dt$days_from_start_of_month > 14, 14, dt$days_from_start_of_month)
  return(dt)
}
 
get_store_item_onpromotion_count <- function(dt){
  count <- dt %>%
    group_by(item_nbr, store_nbr, onpromotion) %>%
    summarize(store_item_onpromotion_count = n())
  return(count)
}


library(dplyr)

# Function to create rolling mean features
create_rolling_means <- function(data, group_cols, window_sizes, mean_col_name) {
  
  # Calculate rolling means for each window size
  for (window_size in window_sizes) {
    
    # Create a new variable name
    var_name <- paste0("rolling_mean_", window_size)
    
    # Calculate the rolling mean for each variable
    data <- data %>%
      group_by(across(all_of(group_cols))) %>%
      mutate({{var_name}} := rollmeanr({{mean_col_name}}, window_size, fill = NA)) %>%
      ungroup()
  }
  
  return(data)
}

calc_rolling_mean_sales_item_store <- function(df, window, mean_col_name="rolling_avg_sales") {
  # Create a data frame with all combinations of date, item_nbr, and store_nbr
  date_range <- seq(min(df$date), max(df$date), by = "day")
  item_store_comb <- df %>% 
    select(item_nbr, store_nbr) %>% 
    distinct() %>% 
    expand(date = date_range, item_nbr, store_nbr) %>% 
    arrange(item_nbr, store_nbr, date)
 
  # Join the data frames to fill in missing values and calculate rolling mean
  df_with_rolling_mean <- item_store_comb %>% 
    left_join(df, by = c("date", "item_nbr", "store_nbr")) %>% 
    arrange(item_nbr, store_nbr, date) %>% 
    mutate(prev_sales = lag(unit_sales, default = 0)) %>%
    group_by(item_nbr, store_nbr) %>%
    mutate(prev_sales = ifelse(is.na(prev_sales), 0, prev_sales)) %>% 
    mutate(rolling_avg_sales = rollmeanr(prev_sales, window, fill = 0, align = "right")) %>% 
    ungroup() %>% 
    mutate(rolling_avg_sales = ifelse(is.na(rolling_avg_sales), 0, rolling_avg_sales)) %>%
    select(-prev_sales)
  
  rolling_avg_col_name <- paste0(mean_col_name, "_", window)
  df_with_rolling_mean <- df_with_rolling_mean %>% rename(!!rolling_avg_col_name := rolling_avg_sales)

  return(as.data.table(df_with_rolling_mean))
  
}

calc_rolling_sum_promo_item_store <- function(df, window, mean_col_name="rolling_sum_promo") {
  # Create a data frame with all combinations of date, item_nbr, and store_nbr
  date_range <- seq(min(df$date), max(df$date), by = "day")
  item_store_comb <- df %>% 
    select(item_nbr, store_nbr) %>% 
    distinct() %>% 
    expand(date = date_range, item_nbr, store_nbr) %>% 
    arrange(item_nbr, store_nbr, date)
  
  # Join the data frames to fill in missing values and calculate rolling mean
  df_with_rolling_mean <- item_store_comb %>% 
    left_join(df, by = c("date", "item_nbr", "store_nbr")) %>% 
    arrange(item_nbr, store_nbr, date) %>% 
    mutate(prev_promo = lag(onpromotion, default = 0)) %>%
    group_by(item_nbr, store_nbr) %>%
    mutate(prev_promo = ifelse(is.na(prev_promo), 0, prev_promo)) %>% 
    mutate(rolling_sum_promo = rollsumr(prev_promo, window, fill = 0, align = "right")) %>% 
    ungroup() %>% 
    mutate(rolling_sum_promo = ifelse(is.na(rolling_sum_promo), 0, rolling_sum_promo)) %>%
    select(-prev_promo)
  
  rolling_avg_col_name <- paste0(mean_col_name, "_", window)
  df_with_rolling_mean <- df_with_rolling_mean %>% rename(!!rolling_avg_col_name := rolling_sum_promo)
  
  return(as.data.table(df_with_rolling_mean))
  
}



add_rolling_mean_sales_item_store <- function(dt, rolling_means) {
  dt <- dt %>%
    left_join(rolling_means)
  #mutate(!!mean_col_name := coalesce(ifelse(is.na(as.numeric(!!mean_col_name)), 0, as.numeric(!!mean_col_name)), 0))
  return(dt)
}



  