library(lightgbm)
library(tidyverse)
library(tsibble)
library(fable)
library(lubridate)
library(data.table)
library(dplyr)
library(Metrics)
library(zoo)
library(zip)

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

tr <- fread("data/train_20170201_onwards.csv", na.strings="")

# TRAIN #########################################################################################

train <- tr[tr$date >= "2017-04-01",]
rm(tr)
gc()
train <- process_data(train)
print(paste0("Processed Train. ", min(train$date), " to ", max(train$date)))

# log1p the unit_sales to deal with the distribution of the target;
# lots of low values close to 0, but then a few very large positive (and some negative) values
train$unit_sales <- log1p(ifelse(train$unit_sales>0,train$unit_sales,0))
# we have by default added rolling_avg_sales_1 in the data wrangling process. log1p that as well
train$rolling_avg_sales_1  <- log1p(ifelse(train$rolling_avg_sales_1 >0,train$rolling_avg_sales_1 ,0))

# add weights based on perishable
train$weights <- ifelse(train$perishable == 1, 1.25, 1)

train <- add_day_of_week(train)
train <- add_day_of_month(train)
train <- add_is_soon_after_public_payday(train)
train <- add_is_soon_after_payday(train)
train <- add_days_from_start_of_month_first14(train)
train <- add_days_from_15th(train)
train <- add_is_start_of_month(train)
train <- add_is_perishable_promotion(train)

# rolling 1 is there by default
#train <- calc_rolling_mean_sales_item_store(train, 1)
train <- calc_rolling_mean_sales_item_store(train, 3)
train <- calc_rolling_mean_sales_item_store(train, 7)
train <- calc_rolling_mean_sales_item_store(train, 14)

train <- calc_rolling_sum_promo_item_store(train, 7)

# exclude rolling data "warming up" phase
train <- train[train$date > (min(train$date) + lubridate::days(14)),]






pred_features <- c("rolling_avg_sales_1", "rolling_avg_sales_3", "rolling_avg_sales_7", "rolling_avg_sales_14", "rolling_sum_promo_7",
                   "onpromotion", "is_holiday", "day_of_week", "day_of_month", "is_perishable_promotion", "dcoilwtico")

categorical_features <- c("day_of_week", "day_of_month")


# do cv, get best hyper params
cv_results <- do_cv(train, num_windows = 3, window_length = 15, pred_features = pred_features, categorical_features = categorical_features)
feature_importance <- cv_results$cv_feature_importances
write.csv(feature_importance, "analysis/cv_feature_importance.csv", row.names=F)
cv_error_metrics <- cv_results$cv_error_metrics
write.csv(cv_error_metrics, "analysis/cv_error_metrics.csv", row.names=F)



mean_over_cv_splits <- cv_error_metrics %>% 
  group_by(hp_combo) %>%
  summarise(
    mean_tr_rmse = mean(tr_rmse), 
    mean_tr_nwrmsle = mean(tr_nwrmsle),
    mean_tr_nwrmsle_uw = mean(tr_nwrmsle_uw), 
    mean_tr_mae = mean(tr_mae), 
    mean_te_rmse = mean(te_rmse), 
    mean_te_nwrmsle = mean(te_nwrmsle),
    mean_te_nwrmsle_uw = mean(te_nwrmsle_uw), 
    mean_te_mae = mean(te_mae)
  ) %>%
  ungroup()

best_hp_combo <- mean_over_cv_splits[mean_over_cv_splits$mean_te_nwrmsle == min(mean_over_cv_splits$mean_te_nwrmsle),]$hp_combo
best_hyper <- cv_error_metrics[cv_error_metrics$hp_combo == best_hp_combo,][1,]


train_lgb <- create_lgb_dataset(train, pred_features, categorical_features = categorical_features)

# leave the last 15 days for our validation set for this final train
val_dt <- train[train$date >= (max(train$date) - lubridate::days(15)) & train$id != "NA",]
val_dt <- apply_train_features_forward(train[train$date < (max(train$date) - lubridate::days(15)),], val_dt)
valid_lgb <- create_lgb_dataset(val_dt, pred_features, categorical_feature = categorical_features)


# train the final model using best hyper params from above
light_gbn_tuned <- lgb.train(
  params = list(
    objective = "regression", 
    metric = "rmse",
    max_depth = best_hyper$max_depth,
    num_leaves = best_hyper$num_leaves,
    num_iterations = best_hyper$num_iterations,
    learning_rate = best_hyper$learning_rate,
    verbose = 1,
    weight_column = "weights"
  ), 
  early_stopping_rounds = best_hyper$early_stopping_rounds,
  valids = list(test = valid_lgb),
  data = train_lgb
)

# check the feature importance scores of the final fit
feature_imp <- lgb.importance(light_gbn_tuned, percentage = TRUE)
print(feature_imp)

# create and score train predictions
train_preds <- get_predictions(train, light_gbn_tuned)
tr_error_metrics <- get_error_metrics(expm1(train_preds$unit_sales), expm1(train_preds$unit_sales_pred), train_preds$weights)
tr_error_metrics

# create and score validation predictions
val_preds <- get_predictions(val_dt, light_gbn_tuned)
val_error_metrics <- get_error_metrics(expm1(val_preds$unit_sales), expm1(val_preds$unit_sales_pred), val_preds$weights)
val_error_metrics

# store validation predictions for evaluation
write.csv(cbind(val_dt, val_preds), "analysis/final_fit_validation_preds.csv", row.names = F)




#### TEST ########################################################################################

te <- fread("data/test_joined.csv", na.strings="")
test_ids <- te$id
test <- process_data(te)
print(paste0("Processed Test. ", min(test$date), " to ", max(test$date)))
test <- add_day_of_week(test)
test <- add_day_of_month(test)
test <- add_days_from_start_of_month_first14(test)
test <- add_days_from_15th(test)
test <- add_is_soon_after_public_payday(test)
test <- add_is_soon_after_payday(test)
test <- add_is_start_of_month(test)
test <- add_is_perishable_promotion(test)
test <- apply_train_features_forward(train, test)


# create test predictions
test_preds <-data.table(id = test_ids,
             unit_sales = predict(light_gbn_tuned, as.matrix(test[, ..pred_features])))
# undo the log1p
test_preds$unit_sales <- expm1(test_preds$unit_sales)
test_preds[unit_sales < 0, unit_sales := 0]


write.csv(test_preds, "submission/submission.csv", row.names = F)
zipr(zipfile = "submission/submission.zip", files = "submission/submission.csv")
