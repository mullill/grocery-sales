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

add_last_rolling_class_store_means <- function(train, test, window = 14) {
  
  # make sure we are in order (most recent date last)
  train <- train %>% arrange(date)
  
  # set the dynamic column name
  rolling_avg_col_name <- paste0("rolling_avg_class_store_sales_", window)
  
  # if we already have this column, remove it
  if(rolling_avg_col_name %in% names(test)){
    test <- test %>% select(-!!sym(rolling_avg_col_name))
  }
  
  # get the last rolling average sales for each class and store
  last_rolling_avg_sales <- train %>%
    group_by(class, store_nbr) %>%
    slice_tail(n = 1) %>%
    select(class, store_nbr, !!rolling_avg_col_name)
  
  # join the last rolling average sales to the test data and fill NAs with 0
  test <- test %>%
    left_join(last_rolling_avg_sales, by = c("class", "store_nbr"))
  
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

add_store_promotion_perc_diff <- function(dt) {
  
  store_promotion_perc_diff <- dt %>%
    group_by(store_nbr, onpromotion) %>%
    summarize(mean_unit_sales = mean(unit_sales)) %>%
    pivot_wider(names_from = onpromotion, values_from = mean_unit_sales) %>%
    mutate(store_promotion_perc_diff = (1 - `0`/`1`) * 100)
  
  
  
  dt <- dt %>% left_join(store_promotion_perc_diff[, c("store_nbr", "store_promotion_perc_diff")])
  
  return(dt)
  
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

add_is_start_of_month <- function(dt){
  dt$is_start_of_month <- ifelse(dt$day_of_month < 3, 1, 0)
  return(dt)
}

add_is_soon_after_payday <- function(dt){
  dt$is_soon_after_payday <- ifelse(dt$day_of_month %in% c(1, 2, 3, 4, 16, 17), 1, 0)
  return(dt)
}

add_is_soon_after_public_payday <- function(dt){
  dt$is_soon_after_public_payday <- ifelse(dt$day_of_month %in% c(16, 17), 1, 0)
  return(dt)
}


add_day_of_month <- function(dt){
  dt$day_of_month <- lubridate::day(dt$date)
  return(dt)
}

add_days_from_15th <- function(dt) {
  dt$days_from_15th <- abs(day(dt$date) - 15)
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
  
  # Find the first date with non-zero sales for each item-store combination
  first_sales_date <- df_with_rolling_mean %>%
    group_by(item_nbr, store_nbr) %>%
    summarize(first_sales_date = min(date[which(unit_sales != 0)])) %>%
    ungroup() %>%
    filter(!is.infinite(first_sales_date))
  
  # only return rows from first sale onwards
  df_with_rolling_mean <- df_with_rolling_mean %>%
    left_join(first_sales_date, by = c("item_nbr", "store_nbr")) %>%
    filter(!is.infinite(first_sales_date)) %>%
    filter(date >= first_sales_date)
  
  df_with_rolling_mean <- df_with_rolling_mean %>% select(-first_sales_date)
  rolling_avg_col_name <- paste0(mean_col_name, "_", window)
  df_with_rolling_mean <- df_with_rolling_mean %>% rename(!!rolling_avg_col_name := rolling_avg_sales)

  return(as.data.table(df_with_rolling_mean))

}

calc_rolling_mean_sales_class_store <- function(df, window, mean_col_name="rolling_avg_class_store_sales") {
  # Create a data frame with all combinations of date, class, and store_nbr
  date_range <- seq(min(df$date), max(df$date), by = "day")
  class_store_comb <- df %>% 
    select(class, store_nbr) %>% 
    distinct() %>% 
    expand(date = date_range, class, store_nbr) %>% 
    arrange(class, store_nbr, date)
  
  # Join the data frames to fill in missing values and calculate rolling mean
  df_with_rolling_mean <- class_store_comb %>% 
    left_join(df, by = c("date", "class", "store_nbr")) %>% 
    arrange(class, store_nbr, date) %>% 
    mutate(prev_sales = lag(unit_sales, default = 0)) %>%
    group_by(class, store_nbr) %>%
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
  
  # Find the first date with non-zero sales for each item-store combination
  first_sales_date <- df_with_rolling_mean %>%
    group_by(item_nbr, store_nbr) %>%
    summarize(first_sales_date = min(date[which(unit_sales != 0)])) %>%
    ungroup() %>%
    filter(!is.infinite(first_sales_date))
  
  # only return rows from first sale onwards
  df_with_rolling_mean <- df_with_rolling_mean %>%
    left_join(first_sales_date, by = c("item_nbr", "store_nbr")) %>%
    filter(!is.infinite(first_sales_date)) %>%
    filter(date >= first_sales_date)
  
  df_with_rolling_mean <- df_with_rolling_mean %>% select(-first_sales_date)
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



create_promotion_factor <- function(dt){
  
  # compute average sales for each item and store
  avg_sales <- dt[, .(avg_sales = mean(unit_sales)), by = .(store_nbr, item_nbr)]
  
  # compute average sales for each item and store when on promotion
  avg_sales_promo <- dt[onpromotion == TRUE, .(avg_sales_promo = mean(unit_sales)), by = .(store_nbr, item_nbr)]
  
  # compute promotion factor as the ratio of average sales when on promotion to average sales
  promo_factor <- merge(avg_sales, avg_sales_promo, by = c("store_nbr", "item_nbr"), all.x = TRUE)
  promo_factor[, promo_factor := ifelse(is.na(avg_sales_promo), 1, avg_sales_promo / avg_sales)]
  
  # fill missing promotion factor with 1
  promo_factor[is.na(promo_factor)] <- 1
  
  # merge promotion factor back to the original data table
  dt <- merge(dt, promo_factor, by = c("store_nbr", "item_nbr"), all.x = TRUE)
  
  # remove unnecessary columns
  dt <- dt[, !c("avg_sales", "avg_sales_promo")]
  
  return(dt)
}


add_ever_been_onpromotion <- function(dt){
  
  # sort by store_nbr, item_nbr, and date
  dt <- dt %>%
    arrange(store_nbr, item_nbr, date)
  
  # create a new column indicating whether onpromotion has ever been 1 before the current row's date
  dt <- dt %>%
    group_by(store_nbr, item_nbr) %>%
    mutate(onpromotion_before = cummax(lag(onpromotion, default = 0))) %>%
    as.data.table()
  
  # print the results
  return(dt)
  
}

#NOT WORKING
add_days_since_last_onpromotion <- function(dt) {
  
  # sort by store_nbr, item_nbr, and date
  dt <- dt %>%
    arrange(store_nbr, item_nbr, date)
  
  # create a new column indicating the date when `onpromotion` was last 1 for the given `store_nbr` and `item_nbr`
  dt <- dt %>%
    group_by(store_nbr, item_nbr) %>%
    mutate(last_onpromotion_date = lag(ifelse(onpromotion == 1, date, NA), default = first(date))) %>%
    ungroup()
  
  # create a new column indicating the number of days since the last onpromotion
  dt <- dt %>%
    mutate(days_since_last_onpromotion = as.numeric(date - last_onpromotion_date))
  
  # set all NAs to a large number (e.g. 99999)
  dt[is.na(days_since_last_onpromotion), days_since_last_onpromotion := 99999]
  
  # print the results
  return(dt)
  
}

add_mean_store_onpromotion_pct_diff <- function(df) {
  # compute the mean percentage difference for each store_nbr when onpromotion is 0 or 1
  df_mean <- df %>%
    group_by(store_nbr, onpromotion) %>%
    summarize(mean_store_onpromotion_pct_diff = mean((unit_sales - mean(unit_sales))/mean(unit_sales), na.rm = TRUE))
  
  # join the mean_pct_diff column to the original data frame
  df <- left_join(df, df_mean, by = c("store_nbr", "onpromotion"))
  
  # return the modified data frame
  return(df)
}

add_is_perishable_promotion <- function(dt){
  
  dt$is_perishable_promotion <- dt$onpromotion * dt$perishable
  return(dt)
  
  
}


library(zoo)

add_rolling_store_performance <- function(dt, n) {
  
  # sort by date and store_nbr
  dt <- dt[order(date, store_nbr)]
  
  # calculate the rolling sum of unit_sales for each store_nbr
  dt[, rolling_sum := rollapply(unit_sales, n, sum, align = "right", partial = TRUE), by = store_nbr]
  
  # calculate the percentage change in unit_sales for each store_nbr over the last n days
  dt[, rolling_pct_change := (rolling_sum / lag(rolling_sum, n)) - 1, by = store_nbr]
  
  # return the results
  return(dt)
  
}

add_rolling_store_performance <- function(dt, n) {
  dt %>%
    group_by(store_nbr, date_shifted = date - n) %>%
    summarize(rolling_sales = sum(unit_sales), .groups = 'drop') %>%
    mutate(rolling_pct_change = (rolling_sales/lag(rolling_sales, default = first(rolling_sales))) - n) %>%
    as.data.table()
}




add_days_on_promotion <- function(dt) {
  
  # Create a new column indicating the start of each period of onpromotion
  dt <- dt %>% 
    group_by(store_nbr, item_nbr) %>% 
    mutate(
      onpromotion_start = ifelse(onpromotion == 1 & lag(onpromotion, default = 0) == 0, 1, 0)
    )
  
  # Calculate the cumulative sum of onpromotion within each group, starting from the onpromotion_start column
  dt <- dt %>% 
    group_by(store_nbr, item_nbr, cumsum(onpromotion_start)) %>% 
    mutate(
      days_on_promotion = cumsum(onpromotion),
      onpromotion_start = ifelse(onpromotion_start == 1, 1, 0),
      days_on_promotion = ifelse(onpromotion == 0, 0, days_on_promotion)
    ) %>% 
    ungroup() %>% 
    select(-onpromotion_start) %>%
    as.data.table()
  
  dt <- dt[, !"cumsum(onpromotion_start)", with=FALSE]
  
  return(dt)
  
}





carry_over_days_on_promotion <- function(train_dt, test_dt) {
  
  test_dt <- add_days_on_promotion(test_dt)
  
  # Extract the last known information for each store and item combination from the "train" set
  last_known_info <- train_dt %>% 
    filter(date == max(train_dt$date)) %>% 
    select(store_nbr, item_nbr, days_on_promotion) %>% 
    rename(last_days_on_promotion = days_on_promotion)
  
  # Merge the last known information with the "test" set
  merged_dt <- test_dt %>% 
    left_join(last_known_info, by = c("store_nbr", "item_nbr")) %>%
    mutate(last_days_on_promotion = ifelse(onpromotion == 0, 0, last_days_on_promotion))
 
  
}



