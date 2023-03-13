library(lightgbm)
library(tidyverse)
library(tsibble)
library(fable)
library(lubridate)
library(data.table)
library(dplyr)
library(Metrics)
library(zoo)

source("util.R")
source("feature_engineering.R")



process_data <- function(dt){

  dt <- dt %>% mutate(date = ymd(date))
  # set onpromotion to integer
  dt$onpromotion = as.integer(dt$onpromotion)
  # when onpromotion is NA, just set it to 0
  dt[is.na(onpromotion), onpromotion := 0]
  
  return(dt)
  
}

create_lgb_dataset <- function(dt, pred_features = c("onpromotion")){
  
  features <- setdiff(pred_features, "unit_sales")
  #print(head(as.matrix(dt[, features, with = FALSE])))
  dt_ds <- lgb.Dataset(as.matrix(dt[, features, with = FALSE]), label = dt$unit_sales)
  
  return(dt_ds)
}


tr <- fread("data/train_2017.csv", na.strings="",
               col.names=c("id","date","store_nbr","item_nbr","unit_sales","onpromotion"))
te <- fread("data/test.csv", na.strings="",
               col.names=c("id","date","store_nbr","item_nbr","onpromotion"))


# TRAIN #############################


train <- process_data(tr)
# log1p the unit_sales to deal with the distribution of the target;
# lots of low values close to 0, but then a few very large positive (and some negative) values
train$unit_sales <- log1p(ifelse(train$unit_sales>0,train$unit_sales,0))

simple_means <- get_simple_means(train)
train <- add_avg_unit_sales(train, simple_means)
train$month_number <- lubridate::month(train$date)
train <- add_day_of_week(train)
train <- add_isweekend(train)

train <- add_rolling_mean_sales(train, window = 14)
# only start using the data from when we have numbers coming in for rolling_mean_sales
train <- train[train$rolling_avg_sales > 0, ]


train <- train %>% arrange(desc(date))





# TEST ##############################

test_ids <- te$id
test <- process_data(te)
test <- add_avg_unit_sales(test, simple_means)
test$month_number <- lubridate::month(test$date)
test <- add_day_of_week(test)
test <- add_isweekend(test)

test <- add_last_rolling_means(train, test)
test[is.na(rolling_avg_sales), rolling_avg_sales := 0]


pred_features <- c("avg_unit_sales", "month_number", "day_of_week", "isweekend", "rolling_avg_sales", "onpromotion")
dtrain <- create_lgb_dataset(data.table(train), pred_features)


param_grid <- expand.grid(learning_rate = c(0.1, 0.01))

params <- list(
  objective = "regression",
  metric = "rmse",
  boost_from_average = FALSE,
  learning_rate = 0.06,
  max_depth = -1,
  min_split_gain = 0.0,
  min_child_weight = 0.001,
  min_child_samples = 20,
  subsample = 1.0,
  subsample_freq = 0,
  colsample_bytree = 1.0,
  reg_alpha = 0.0,
  reg_lambda = 0.0,
  nthread = -1,
  verbose = -1,
  seed = 42,
  hyper_params = param_grid
)

cv_results <- lgb.cv(
  params = params
  , data = dtrain
  , nrounds = 10L
  , nfold = 7L
)

#best_hyperparams <- param_grid[cv_results$best_iter, ]



final_model <- lgb.train(dtrain, params = params, nrounds = cv_results$best_iter)

# features used
feature_imp <- lgb.importance(final_model, percentage = TRUE)
features_used <- feature_imp$Feature
feature_imp

train_preds <- cbind(train[1:10000,], unit_sales_pred=predict(final_model, as.matrix(train[1:10000, ..pred_features])))
train_preds$unit_sales_pred <- expm1(train_preds$unit_sales_pred)

Metrics::rmse(train_preds$unit_sales, train_preds$unit_sales_pred)




test_preds <- data.table(id=test_ids, unit_sales=predict(final_model, as.matrix(test[, ..pred_features])))
# undo the log1p
test_preds$unit_sales <- expm1(test_preds$unit_sales)



write.csv(test_preds, "submission/submission.csv", row.names = F)
