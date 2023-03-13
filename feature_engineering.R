
add_rolling_mean_sales <- function(dt, window = 14){
  
  dt <- dt %>%
    arrange(date, item_nbr, store_nbr) %>% # Sort by item_nbr, store_nbr and date
    group_by(item_nbr, store_nbr) %>% # Group by item_id
    mutate(rolling_avg_sales = rollapply(unit_sales, width = window, FUN = mean, fill = NA, align = "right")) %>%
    ungroup() %>% as.data.table()
  
  dt[is.na(rolling_avg_sales), rolling_avg_sales := 0]
  
  return(dt)
  
}

add_last_rolling_means <- function(train, test){
  
  last_rolling_avg_sales <- train %>%
    group_by(item_nbr, store_nbr) %>%
    slice_tail(n = 1) %>%
    select(item_nbr, store_nbr, rolling_avg_sales)
  
  test <- test %>%
    left_join(last_rolling_avg_sales, by = c("item_nbr", "store_nbr")) %>%
    mutate(rolling_avg_sales = ifelse(is.na(rolling_avg_sales), 0, rolling_avg_sales))
  
  return(test)

}



add_day_of_week <- function(dt){
  
  dt$day_of_week <- lubridate::wday(dt$date, week_start = 1)
  
  return(dt)
  
}

add_isweekend <- function(dt){
  
  dt$isweekend <- as.integer(lubridate::wday(dt$date, week_start = 1) %in% c(6,7))
  
  return(dt)
  
}




get_simple_means <- function(dt){
  
  avg_sales <- dt %>%
    group_by(item_nbr, store_nbr, onpromotion) %>%
    summarize(avg_unit_sales = mean(unit_sales))

  return(avg_sales)
  
}


add_avg_unit_sales <- function(dt, means){
  
  dt <- dt %>% left_join(means, by=c("item_nbr","store_nbr","onpromotion")) %>% as.data.table()
  
  dt[is.na(dt$avg_unit_sales)]$avg_unit_sales <- 0
  
  return(dt)
  
}
 
  