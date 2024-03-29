library(data.table)
library(dplyr)
library(ggplot2)

source("util.R")
source("feature_engineering.R")

add_stores_and_holidays_features <- function(dt){

  holidays <- fread("data/holidays_events.csv",  na.strings="",
                    col.names=c("date", "type", "locale", "locale_name", "description", "transferred"))
  holidays <- holidays %>% select(-description)
  holidays$transferred = as.integer(holidays$transferred)
  
  stores <- fread("data/stores.csv",  na.strings="")
  
  dt <- left_join(dt, stores)
  
  #train$is_holiday <- as.integer(lubridate::wday(train$date, week_start = 1) %in% c(6,7))
  dt$is_holiday <- 0
  
  for (i in 1:nrow(holidays)) {
    d <- holidays$date[i]
    t <- holidays$type[i]
    l <- holidays$locale[i]
    n <- holidays$locale_name[i]
    
    if (t != 'Work Day') {
      if (l == 'National') {
        dt$is_holiday[dt$date == d] <- 1
      } else if (l == 'Regional') {
        dt$is_holiday[dt$date == d & dt$state == n] <- 1
      } else {
        dt$is_holiday[dt$date == d & dt$city == n] <- 1
      }
    } else {
      dt$is_holiday[dt$date == d] <- 0
    }
  }
  
  dt <- dt %>% select(-city, -state)
  
}

add_item_features <- function(dt){
  items <- fread("data/items.csv", na.strings="")
  dt <- left_join(dt, items)
  return(dt)
}

add_transaction_features <- function(dt){
  transactions <- fread("data/transactions.csv", na.strings="")
  transactions <- transactions %>% mutate(date = ymd(date))
  dt <- left_join(dt, transactions)
  return(dt)
}

add_oil_features <- function(dt){
  oil <- fread("data/oil.csv", na.strings="")
  oil <- oil %>% mutate(date = ymd(date))
  dt <- left_join(dt, oil)
  
  # where we don't have an oil value
  dt$dcoilwtico <- na.locf(dt$dcoilwtico, na.rm = FALSE)
  
  return(dt)
}

process_data <- function(dt){
  
  dt <- dt %>% mutate(date = ymd(date))
  # set onpromotion to integer
  dt$onpromotion = as.integer(dt$onpromotion)
  # when onpromotion is NA, just set it to 0
  dt[is.na(onpromotion), onpromotion := 0]
  
  return(as.data.table(dt))
  
}




################## TRAIN ###########################################################################
train <- fread("data/train.csv", na.strings="",
               col.names=c("id","date","store_nbr","item_nbr","unit_sales","onpromotion"))

train <- train[train$date >= "2017-02-01",]
train <- process_data(train)

# this step will "sparsify" the data, filling in blanks where there were no sales for a given store/item
train <- calc_rolling_mean_sales_item_store(train, 1)

nrow(train[is.na(train$id),]) / nrow(train)

train[is.na(unit_sales), unit_sales:= 0]
train[is.na(onpromotion), onpromotion:= 0]

train <- add_stores_and_holidays_features(train)
train <- add_item_features(train)
train <- add_transaction_features(train)
train <- add_oil_features(train)

write.csv(train, "data/train_20170201_onwards.csv", row.names=F)




#Get a random 20% of observations
train_sample <- train[sample(.N, .2 * .N), ]
train_sample <- process_data(train_sample)
train_sample <- add_stores_and_holidays_features(train_sample)
train_sample <- add_item_features(train_sample)
train_sample <- add_transaction_features(train_sample)
train_sample <- add_oil_features(train_sample)

write.csv(train_sample, "data/train_sample.csv", row.names=F)
rm(train_sample)
gc()



#### TEST ##########################################################################################

test <- fread("data/test.csv", na.strings="")

test <- process_data(test)

test <- add_stores_and_holidays_features(test)
test <- add_item_features(test)
test <- add_transaction_features(test)
test <- add_oil_features(test)

write.csv(test, "data/test_joined.csv", row.names=F)










