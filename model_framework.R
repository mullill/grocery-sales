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
  
  return(as.data.table(dt))
  
}

create_lgb_dataset <- function(dt, pred_features = c("onpromotion")){
  
  features <- setdiff(pred_features, "unit_sales")
  #print(head(as.matrix(dt[, features, with = FALSE])))
  dt_ds <- lgb.Dataset(as.matrix(dt[, features, with = FALSE]), label = dt$unit_sales)
  
  return(dt_ds)
}

tr <- fread("data/train_2017_simple.csv", na.strings="")
te <- fread("data/test.csv", na.strings="")


# TRAIN #########################################################################################

train <- process_data(tr)
# log1p the unit_sales to deal with the distribution of the target;
# lots of low values close to 0, but then a few very large positive (and some negative) values
train$unit_sales <- log1p(ifelse(train$unit_sales>0,train$unit_sales,0))

train <- add_day_of_week(train)
train <- add_days_from_start_of_month_first14(train)

store_item_onpromotion_means <- get_mean_sales(train, c("item_nbr", "store_nbr", "onpromotion"), "mean_item_store_promo_unit_sales")
train <- add_mean_sales(train, store_item_onpromotion_means, join_cols = c("item_nbr", "store_nbr", "onpromotion"), mean_col_name = "mean_item_store_promo_unit_sales")
item_onpromotion_means <- get_mean_sales(train, c("item_nbr", "onpromotion"), "mean_item_promo_unit_sales")
train <- add_mean_sales(train, item_onpromotion_means, join_cols = c("item_nbr", "onpromotion"), mean_col_name = "mean_item_promo_unit_sales")
store_item_means <- get_mean_sales(train, c("item_nbr", "store_nbr"), "mean_item_store_unit_sales")
train <- add_mean_sales(train, store_item_means, join_cols = c("item_nbr", "store_nbr"), mean_col_name = "mean_item_store_unit_sales")
item_means <- get_mean_sales(train, c("item_nbr"), "mean_item_unit_sales")
train <- add_mean_sales(train, item_means, join_cols = c("item_nbr"), mean_col_name = "mean_item_unit_sales")

day_of_week_item_means <- get_mean_sales(train, c("item_nbr", "day_of_week"), "mean_item_dow_unit_sales")
train <- add_mean_sales(train, day_of_week_item_means, c("item_nbr", "day_of_week"), "mean_item_dow_unit_sales")
day_of_week_means <- get_mean_sales(train, c("day_of_week"), "mean_dow_unit_sales")
train <- add_mean_sales(train, day_of_week_means, c("day_of_week"), "mean_dow_unit_sales")

days_from_start_of_month_means <- get_mean_sales(train, c("days_from_start_of_month"), "mean_days_from_start_of_month_unit_sales")
train <- add_mean_sales(train, days_from_start_of_month_means, c("days_from_start_of_month"), "mean_days_from_start_of_month_unit_sales")

days_from_start_of_month_item_means <- get_mean_sales(train, c("item_nbr", "days_from_start_of_month"), "mean_days_from_start_of_month_item_unit_sales")
train <- add_mean_sales(train, days_from_start_of_month_item_means, c("item_nbr", "days_from_start_of_month"), "mean_days_from_start_of_month_item_unit_sales")

days_from_start_of_month_store_means <- get_mean_sales(train, c("store_nbr", "days_from_start_of_month"), "mean_days_from_start_of_month_store_unit_sales")
train <- add_mean_sales(train, days_from_start_of_month_store_means, c("store_nbr", "days_from_start_of_month"), "mean_days_from_start_of_month_store_unit_sales")

promo_means <- get_mean_sales(train, c("onpromotion"), "mean_promo_unit_sales")
train <- add_mean_sales(train, promo_means, c("onpromotion"), "mean_promo_unit_sales")



train <- add_is_weekend(train)
#train <- add_is_weekend_workday(train)
train <- add_rolling_mean_sales(train, window = 3)
train <- add_rolling_mean_sales(train, window = 7)
train <- add_rolling_mean_sales(train, window = 14)
train <- add_rolling_mean_sales(train, window = 30)

train[rolling_avg_sales_3 == 0, rolling_avg_sales_3 := mean_item_store_promo_unit_sales]
train[rolling_avg_sales_3 == 0, rolling_avg_sales_3 := mean_item_promo_unit_sales]
train[rolling_avg_sales_3 == 0, rolling_avg_sales_3 := mean_item_unit_sales ]
train[rolling_avg_sales_7 == 0, rolling_avg_sales_7 := rolling_avg_sales_3]
train[rolling_avg_sales_14 == 0, rolling_avg_sales_14 := rolling_avg_sales_7]
train[rolling_avg_sales_30 == 0, rolling_avg_sales_30 := rolling_avg_sales_14]


#train <- add_is_national_holiday(train)
#train <- add_days_to_xmas_last30(train)
#train <- add_days_from_start_of_month_first14(train)

# TEST ########################################################################################

test_ids <- te$id
test <- process_data(te)
test <- add_day_of_week(test)
test <- add_days_from_start_of_month_first14(test)

test <- add_mean_sales(test, store_item_onpromotion_means, join_cols = c("item_nbr", "store_nbr", "onpromotion"), mean_col_name = "mean_item_store_promo_unit_sales")
test <- add_mean_sales(test, item_onpromotion_means, join_cols = c("item_nbr", "onpromotion"), mean_col_name = "mean_item_promo_unit_sales")
test <- add_mean_sales(test, store_item_means, join_cols = c("item_nbr", "store_nbr"), mean_col_name = "mean_item_store_unit_sales")
test <- add_mean_sales(test, item_means, join_cols = c("item_nbr"), mean_col_name = "mean_item_unit_sales")
# impute if missing
test[mean_item_store_promo_unit_sales == 0, mean_item_store_promo_unit_sales := mean_item_promo_unit_sales]
test[mean_item_store_promo_unit_sales == 0, mean_item_store_promo_unit_sales := mean_item_unit_sales]



test <- add_mean_sales(test, day_of_week_item_means, c("item_nbr", "day_of_week"), "mean_item_dow_unit_sales")
test <- add_mean_sales(test, day_of_week_means, c("day_of_week"), "mean_dow_unit_sales")
test <- add_mean_sales(test, days_from_start_of_month_means, c("days_from_start_of_month"), "mean_days_from_start_of_month_unit_sales")
test <- add_mean_sales(test, days_from_start_of_month_item_means, c("item_nbr", "days_from_start_of_month"), "mean_days_from_start_of_month_item_unit_sales")
test <- add_mean_sales(test, days_from_start_of_month_store_means, c("store_nbr", "days_from_start_of_month"), "mean_days_from_start_of_month_store_unit_sales")

test <- add_mean_sales(test, promo_means, c("onpromotion"), "mean_promo_unit_sales")

#test <- add_is_weekend_workday(test)
test <- add_last_rolling_means(train, test, window = 3)
test[rolling_avg_sales_3 == 0, rolling_avg_sales_3 := mean_item_store_promo_unit_sales]
test[rolling_avg_sales_3 == 0, rolling_avg_sales_3 := mean_item_promo_unit_sales]
test[rolling_avg_sales_3 == 0, rolling_avg_sales_3 := mean_item_unit_sales ]

test <- add_last_rolling_means(train, test, window = 7)
test[rolling_avg_sales_7 == 0, rolling_avg_sales_7 := rolling_avg_sales_3]
test <- add_last_rolling_means(train, test, window = 14)
test[rolling_avg_sales_14 == 0, rolling_avg_sales_14 := rolling_avg_sales_7]
test <- add_last_rolling_means(train, test, window = 30)
test[rolling_avg_sales_30 == 0, rolling_avg_sales_30 := rolling_avg_sales_14]



#test <- add_last_rolling_means(train, test, window = 30)
#test <- add_is_national_holiday(test)
#test <- add_days_to_xmas_last30(test)
#test <- add_days_from_start_of_month_first14(test)

# they might be 0, we probably want to then override that with the simple average

pred_features <- c('onpromotion','rolling_avg_sales_14','day_of_week','days_from_start_of_month','mean_item_store_promo_unit_sales','mean_item_promo_unit_sales','mean_item_store_unit_sales','mean_item_unit_sales','mean_item_dow_unit_sales','mean_dow_unit_sales','mean_days_from_start_of_month_unit_sales', 
                   'mean_days_from_start_of_month_item_unit_sales','mean_days_from_start_of_month_store_unit_sales','mean_promo_unit_sales')

pred_featues <- c("rolling_avg_sales_14", "rolling_avg_sales_3", "rolling_avg_sales_7", "rolling_avg_sales_30")

                  
dtrain <- create_lgb_dataset(data.table(train), pred_features)


param_grid <- expand.grid(learning_rate = c(0.1, 0.01))

num_leaves = 2^5-2
max_depth = round(log(num_leaves) / log(2),0)

params <- list(
  objective = "regression",
  num_leaves = num_leaves,
  metric = "rmse",
  boost_from_average = FALSE,
  learning_rate = 0.1,
  max_depth = max_depth,
  # min_split_gain = 0.0,
  # min_child_weight = 0.001,
  # min_child_samples = 20,
  # subsample = 1.0,
  # subsample_freq = 0,
  # colsample_bytree = 1.0,
  # reg_alpha = 0.0,
  # reg_lambda = 0.0,
  # nthread = -1,
  # verbose = -1,
  seed = 42
  #hyper_params = param_grid
)

cv_results <- lgb.cv(
  params = params
  , data = dtrain
  , nrounds = 10L
  , nfold = 7L
  , early_stopping_rounds = 10
)

#best_hyperparams <- param_grid[cv_results$best_iter, ]



final_model <- lgb.train(dtrain, params = params, nrounds = cv_results$best_iter)

# features used
feature_imp <- lgb.importance(final_model, percentage = TRUE)
features_used <- feature_imp$Feature
feature_imp

train_preds <- as.data.table(cbind(unit_sales = train$unit_sales, unit_sales_pred=predict(final_model, as.matrix(train[, ..pred_features]))))
get_error_metrics(train_preds$unit_sales, train_preds$unit_sales_pred)











test_preds <- data.table(id=test_ids, unit_sales=predict(final_model, as.matrix(test[, ..pred_features])))
# undo the log1p
test_preds$unit_sales <- expm1(test_preds$unit_sales)



write.csv(test_preds, "submission/submission.csv", row.names = F)
