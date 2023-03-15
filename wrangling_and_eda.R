library(data.table)
library(dplyr)

add_stores_and_holidays_features <- function(train){

  holidays <- fread("data/holidays_events.csv",  na.strings="",
                    col.names=c("date", "type", "locale", "locale_name", "description", "transferred"))
  holidays <- holidays %>% select(-description)
  holidays$transferred = as.integer(holidays$transferred)
  
  stores <- fread("data/stores.csv",  na.strings="")
  
  train <- left_join(train, stores)
  
  train$is_dayoff <- as.integer(lubridate::wday(train$date, week_start = 1) %in% c(6,7))
  
  for (i in 1:nrow(holidays)) {
    d <- holidays$date[i]
    t <- holidays$type[i]
    l <- holidays$locale[i]
    n <- holidays$locale_name[i]
    
    if (t != 'Work Day') {
      if (l == 'National') {
        train$is_dayoff[train$date == d] <- 1
      } else if (l == 'Regional') {
        train$is_dayoff[train$date == d & train$state == n] <- 1
      } else {
        train$is_dayoff[train$date == d & train$city == n] <- 1
      }
    } else {
      train$is_dayoff[train$date == d] <- 0
    }
  }
  
  train <- train %>% select(-city, -state)
  
}

add_item_features <- function(train){
 
  items <- fread("data/items.csv", na.strings="")
  
  train <- left_join(train, items)

}




################## TRAIN ###########################################################################

train <- fread("data/train.csv", na.strings="",
               col.names=c("id","date","store_nbr","item_nbr","unit_sales","onpromotion"))

train <- add_stores_and_holidays_features(train)
train <- add_item_features(train)


train_2017 <- train[train$date > "2017-03-01",]
write.csv(train_2017, "data/train_2017_Mar.csv", row.names=F)




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

