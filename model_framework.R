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
  weights <- ifelse(dt$perishable == 1, 1.25, 1)
  
  dt_ds <- lgb.Dataset(as.matrix(dt[, features, with = FALSE]),
                       weights = weights,
                       label = dt$unit_sales)
  
  return(dt_ds)
  
}

tr <- fread("data/train_2017_Mar.csv", na.strings="")
te <- fread("data/test.csv", na.strings="")


# TRAIN #########################################################################################


train <- process_data(tr)
print(paste0("Processed Train. ", min(train$date), " to ", max(train$date)))

# log1p the unit_sales to deal with the distribution of the target;
# lots of low values close to 0, but then a few very large positive (and some negative) values
train$unit_sales <- log1p(ifelse(train$unit_sales>0,train$unit_sales,0))

# rolling 3 is there by default
#train <- calc_rolling_mean_sales_item_store(train, 3)
train <- calc_rolling_mean_sales_item_store(train, 7)
train <- calc_rolling_mean_sales_item_store(train, 14)


train <- calc_rolling_sum_promo_item_store(train, 7)




# do we need to get fancy with imputation?
# should we just cull the first 14 days?


# TEST ########################################################################################

test_ids <- te$id
test <- process_data(te)
print(paste0("Processed Test. ", min(test$date), " to ", max(test$date)))
test <- add_day_of_week(test)
test <- add_days_from_start_of_month_first14(test)

test <- add_last_rolling_means(train, test, window = 3)
test <- add_last_rolling_means(train, test, window = 7)
test <- add_last_rolling_means(train, test, window = 14)

test <- add_last_rolling_sums(train, test, 7)


# imputation strategies????


# they might be 0, we probably want to then override that with the simple average

pred_features <- c('onpromotion','rolling_avg_sales_14','day_of_week','days_from_start_of_month','mean_item_store_promo_unit_sales','mean_item_promo_unit_sales','mean_item_store_unit_sales','mean_item_unit_sales','mean_item_dow_unit_sales','mean_dow_unit_sales','mean_days_from_start_of_month_unit_sales', 
                   'mean_days_from_start_of_month_item_unit_sales','mean_days_from_start_of_month_store_unit_sales','mean_promo_unit_sales')

pred_features <- c("rolling_avg_sales_3", "rolling_avg_sales_7", "rolling_avg_sales_14", "rolling_sum_promo_7", "onpromotion", "")

                  
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
feature_imp

train_preds <- as.data.table(cbind(unit_sales = train$unit_sales, 
                                   unit_sales_pred=predict(final_model, as.matrix(train[, ..pred_features])),
                                   weights = ((train$perishable * .25) + 1)))
get_error_metrics(train_preds$unit_sales, train_preds$unit_sales_pred, train_preds$weights)








test_preds <- data.table(id=test_ids, unit_sales=predict(final_model, as.matrix(test[, ..pred_features])))
# undo the log1p
test_preds$unit_sales <- expm1(test_preds$unit_sales)



write.csv(test_preds, "submission/submission.csv", row.names = F)
