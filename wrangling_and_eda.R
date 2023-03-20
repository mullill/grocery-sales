library(data.table)
library(dplyr)
library(ggplot2)

source("util.R")
source("feature_engineering.R")

add_stores_and_holidays_features <- function(train){

  holidays <- fread("data/holidays_events.csv",  na.strings="",
                    col.names=c("date", "type", "locale", "locale_name", "description", "transferred"))
  holidays <- holidays %>% select(-description)
  holidays$transferred = as.integer(holidays$transferred)
  
  stores <- fread("data/stores.csv",  na.strings="")
  
  train <- left_join(train, stores)
  
  #train$is_holiday <- as.integer(lubridate::wday(train$date, week_start = 1) %in% c(6,7))
  train$is_holiday <- 0
  
  for (i in 1:nrow(holidays)) {
    d <- holidays$date[i]
    t <- holidays$type[i]
    l <- holidays$locale[i]
    n <- holidays$locale_name[i]
    
    if (t != 'Work Day') {
      if (l == 'National') {
        train$is_holiday[train$date == d] <- 1
      } else if (l == 'Regional') {
        train$is_holiday[train$date == d & train$state == n] <- 1
      } else {
        train$is_holiday[train$date == d & train$city == n] <- 1
      }
    } else {
      train$is_holiday[train$date == d] <- 0
    }
  }
  
  train <- train %>% select(-city, -state)
  
}

add_item_features <- function(train){
 
  items <- fread("data/items.csv", na.strings="")
  
  train <- left_join(train, items)

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

train <- train[train$date >= "2017-03-01",]
train <- process_data(train)

# this step will "sparsify" the data, filling in blanks where there were no sales for a given store/item
train <- calc_rolling_mean_sales_item_store(train, 1)

nrow(train[is.na(train$id),]) / nrow(train)

train[is.na(unit_sales), unit_sales:= 0]
train[is.na(onpromotion), onpromotion:= 0]

train <- add_stores_and_holidays_features(train)
train <- add_item_features(train)

write.csv(train, "data/train_2017_Mar.csv", row.names=F)

# Get a random 10% of observations
train_sample <- train[sample(.N, .2 * .N), ]
write.csv(train_sample, "data/train_sample.csv", row.names=F)
rm(train_sample)
gc()



#### TEST ##########################################################################################

test <- fread("data/test.csv", na.strings="")

test <- add_stores_and_holidays_features(test)
test <- add_item_features(test)

write.csv(test, "data/test_joined.csv", row.names=F)










#### FULL TRAIN EDA ###############################################################################

hist(train$unit_sales)
hist(train[train$unit_sales > -100 & train$unit_sales < 500,]$unit_sales)

hist(log1p(train$unit_sales))

ggplot(train, aes(x = unit_sales)) +
  geom_density() +
  scale_x_continuous(limits = c(0, 5000))



ggplot(train, aes(x = unit_sales)) +
  geom_histogram(binwidth = 100) +
  scale_x_continuous(limits = c(0, 5000))
  scale_x_continuous(limits = c(0, 5000))








