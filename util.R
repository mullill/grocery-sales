#NWRMSLE = \sqrt{ \frac{\sum_{i=1}^n w_i \left( \ln(\hat{y}i + 1) - \ln(y_i +1)  \right)^2  }{\sum{i=1}^n w_i}} $$
nrmse <- function(y, pred, weights = rep(1, length(y))) {
  y <- pmax(0, pmin(y, max(y, na.rm = TRUE)))
  pred <- pmax(0, pmin(pred, max(pred, na.rm = TRUE)))
  score <- sum(weights * ((log(pred + 1) - log(y + 1)) ^ 2), na.rm = TRUE) / sum(weights, na.rm = TRUE)
  return(sqrt(score))
}

get_error_metrics <- function(actual, predicted, weights){
  
  mean_actual <- mean(actual)
  mean_pred <- mean(predicted)
  
  rmse = Metrics::rmse(actual, predicted)
  nrmse = nrmse(actual, predicted, weights)
  mae = Metrics::mae(actual, predicted)
  #smape = Metrics::smape(actual, predicted)
  #mape = Metrics::mape(actual, predicted)
  
  # quantiles <- c(0.05, 0.10, 0.50, 0.90, 0.95)
  # ql <- quantileLoss(actual, predicted, quantiles)
  
  return(data.frame(rmse, nrmse, mae))
  
}

create_lgb_dataset <- function(dt, pred_features = c("onpromotion"), categorical_features = c("onpromotion")){

  dt_ds <- lgb.Dataset(as.matrix(dt[, pred_features, with = FALSE]),
                       label = dt$unit_sales, 
                       categorical_feature = categorical_features,
                       weight = dt$weights)
  
  return(dt_ds)
  
}

# given a Training set, we need to apply the features we have created
# "forward" into the Test set
apply_train_features_forward <- function(tr_cv, te_cv){
  
  print("Add Rolling Mean Sales: 1")
  te_cv <- add_last_rolling_means(tr_cv, te_cv, window = 1)
  print("Add Rolling Mean Sales: 3")
  te_cv <- add_last_rolling_means(tr_cv, te_cv, window = 3)
  print("Add Rolling Mean Sales: 7")
  te_cv <- add_last_rolling_means(tr_cv, te_cv, window = 7)
  print("Add Rolling Mean Sales: 14")
  te_cv <- add_last_rolling_means(tr_cv, te_cv, window = 14)
  
  #print("Add Rolling Mean Class/Store Sales: 7")
  #te_cv <- add_last_rolling_class_store_means(tr_cv, te_cv, window = 7)
  
  print("Add Rolling Sum Promos: 7")
  te_cv <- add_last_rolling_sums(tr_cv, te_cv, 7)
  
  return(te_cv)
  
}

# given a Train set, do CV in a rolling fashion
# for each split, we create a tr_cv and te_cv which is 'window_length' days in advance
# we need to apply our features forward from tr_cv onto te_cv and strip what we wouldn't have know
# we loop over our hyper param grid in each split, performing inner cv on the tr_cv, and then
# fitting the final model for te_cv
# we capture all relevant metrics for that final train/test
do_cv <- function(train, num_windows = 3, window_length = 15, pred_features = c("rolling_avg_sales_3", "onpromotion"), 
                  categorical_features = c()){
  
  #grid search
  #create hyperparameter grid
  num_leaves = c(30, 50)
  max_depth = c(10, 50)
  num_iterations = c(25, 50)
  early_stopping_rounds = 10#round(num_iterations * .1,0)
  learning_rate = c(0.01, 0.1, 0.5)
  hyper_grid <- expand.grid(max_depth = max_depth,
                            num_leaves = num_leaves,
                            num_iterations = num_iterations,
                            early_stopping_rounds=early_stopping_rounds,
                            learning_rate = learning_rate)
  
  cv_error_metrics <- NULL
  cv_feature_importances <- NULL
  
  for(i in 1:num_windows){
    print(paste0("CV Split: ", i))
    
    # get tr_cv
    # end = max_date - i*window_length
    # start = min_date + (i-1) * window_length
    start_date <- min(train$date) + days((i-1) * window_length)
    end_date <- max(train$date) - days(i * window_length)
    
    tr_cv <- train[train$date >= start_date & train$date <= end_date]
    print(paste0("tr_cv: ", min(tr_cv$date), " to ", max(tr_cv$date), ". num rows: ", nrow(tr_cv)))
    
    te_cv <- train[train$id != "NA" & train$date > max(tr_cv$date) & train$date <= (max(tr_cv$date) + days(window_length))]
    print(paste0("te_cv: ", min(te_cv$date), " to ", max(te_cv$date), ". num rows: ", nrow(te_cv)))
    
    # prepare te_cv as if it was out-sample ahead
    print("processing te_cv")
    # given our training set, create the test set features
    te_cv <- apply_train_features_forward(tr_cv, te_cv)
    
    train_lgb <- create_lgb_dataset(tr_cv, pred_features, categorical_features)
    test_lgb <- create_lgb_dataset(te_cv, pred_features, categorical_features)
   
    for(j in 1:nrow(hyper_grid)) {
      print(paste0("Hyper Param Combo: ", j))
      print(hyper_grid[j,])
    
      light_gbn_tuned <- lgb.train(
        params = list(
          objective = "regression", 
          metric = "rmse",
          max_depth = hyper_grid$max_depth[j],
          num_leaves = hyper_grid$num_leaves[j],
          num_iterations = hyper_grid$num_iterations[j],
          learning_rate = hyper_grid$learning_rate[j], 
          verbose = 1,
          weight_column = "weights"
        ), 
        early_stopping_rounds = hyper_grid$early_stopping_rounds[j],
        valids = list(test = test_lgb),
        data = train_lgb
      )
      
      # features used
      feature_imp <- cbind(cv_split = i, hp_combo = j, lgb.importance(light_gbn_tuned, percentage = TRUE))
      print(feature_imp)
      feature_imp <- cbind(feature_imp, hyper_grid[j,])
      cv_feature_importances <- rbind(cv_feature_importances, feature_imp)
      
      train_preds <- as.data.table(cbind(unit_sales = tr_cv$unit_sales, 
                                         unit_sales_pred=predict(light_gbn_tuned, as.matrix(tr_cv[, ..pred_features])),
                                         weights = tr_cv$weights))
      tr_cv_error_metrics <- get_error_metrics(expm1(train_preds$unit_sales), expm1(train_preds$unit_sales_pred), train_preds$weights)
      tr_cv_error_metrics <- tr_cv_error_metrics %>% rename_with(~paste0("tr_", .), everything())
      print(tr_cv_error_metrics)
      
      test_preds <- as.data.table(cbind(unit_sales = te_cv$unit_sales, 
                                         unit_sales_pred=predict(light_gbn_tuned, as.matrix(te_cv[, ..pred_features])),
                                         weights = te_cv$weights))
      te_cv_error_metrics <- get_error_metrics(expm1(test_preds$unit_sales), expm1(test_preds$unit_sales_pred), test_preds$weights)
      te_cv_error_metrics <- te_cv_error_metrics %>% rename_with(~paste0("te_", .), everything())
      print(te_cv_error_metrics)
      
      cv_error_metrics <- rbind(cv_error_metrics, cbind(cv_split = i, hp_combo = j, hyper_grid[j,], tr_cv_error_metrics, te_cv_error_metrics))
      
    }
    
  }
  
  return(list(cv_error_metrics=cv_error_metrics, cv_feature_importances=cv_feature_importances))
  
}


# only get predictions for rows which were not "filler" rows where we 
# added 0s for store/items that didn't have a row
get_predictions <- function(dt, model){
  
  preds <- predict(model, as.matrix(dt[dt$id != "NA", ..pred_features]))
  preds_table <- as.data.table(cbind(dt[dt$id != "NA", c("id","unit_sales","weights")], unit_sales_pred = preds))
  preds_table[unit_sales_pred < 0, unit_sales_pred := 0]
  return(preds_table)
  
}
