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
  print(paste0("sanity check:::: Mean Actual: ", round(mean_actual, 2), " - mean_pred: ", round(mean_pred, 2)))
  
  rmse = Metrics::rmse(actual, predicted)
  nrmse = nrmse(actual, predicted, weights)
  mae = Metrics::mae(actual, predicted)
  smape = Metrics::smape(actual, predicted)
  #mape = Metrics::mape(actual, predicted)
  
  # quantiles <- c(0.05, 0.10, 0.50, 0.90, 0.95)
  # ql <- quantileLoss(actual, predicted, quantiles)
  
  return(data.frame(rmse, nrmse, mae, smape))
  
}

create_lgb_dataset <- function(dt, pred_features = c("onpromotion")){
  
  features <- setdiff(pred_features, "unit_sales")
  weights <- ifelse(dt$perishable == 1, 1.25, 1)
  
  dt_ds <- lgb.Dataset(as.matrix(dt[, features, with = FALSE]),
                       weights = weights,
                       label = dt$unit_sales)
  
  return(dt_ds)
  
}

# given a Train set, do CV in a rolling fashion
# for each split, we create a tr_cv and te_cv which is 'window_length' days in advance
# we need to apply our features forward from tr_cv onto te_cv and strip what we wouldn't have know
# we loop over our hyper param grid in each split, performing inner cv on the tr_cv, and then
# fitting the final model for te_cv
# we capture all relevant metrics for that final train/test
do_cv <- function(train, num_windows = 3, window_length = 15, pred_features = c("rolling_avg_sales_3")){
                                                                                #"rolling_avg_sales_7", 
                                                                                #"rolling_avg_sales_14", 
                                                                                #"rolling_sum_promo_7",
                                                                                #"onpromotion")){
  
  #grid search
  #create hyperparameter grid
  num_leaves = 30
  max_depth = 5
  num_iterations = seq(10, 20, 10)
  early_stopping_rounds = 10#round(num_iterations * .1,0)
  learning_rate = c(0.01, 0.1)
  hyper_grid <- expand.grid(max_depth = max_depth,
                            num_leaves = num_leaves,
                            num_iterations = num_iterations,
                            early_stopping_rounds=early_stopping_rounds,
                            learning_rate = learning_rate)
  
  
  cv_scores <- NULL
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
    
    te_cv <- train[train$date > max(tr_cv$date) & train$date <= (max(tr_cv$date) + days(window_length))]
    print(paste0("te_cv: ", min(te_cv$date), " to ", max(te_cv$date), ". num rows: ", nrow(te_cv)))
    
    # prepare te_cv as if it was out-sample ahead
    print("processing te_cv")
    te_cv <- add_last_rolling_means(tr_cv, te_cv, window = 3)
    # te_cv <- add_last_rolling_means(tr_cv, te_cv, window = 7)
    # te_cv <- add_last_rolling_means(tr_cv, te_cv, window = 14)
    # te_cv <- add_last_rolling_sums(tr_cv, te_cv, 7)
    
    
    for(j in 1:nrow(hyper_grid)) {
      print(paste0("Hyper Param Combo: ", j))
      print(hyper_grid[j,])
    
      train_lgb <- create_lgb_dataset(tr_cv, pred_features)
      
      light_gbn_tuned <- lgb.train(
        params = list(
          objective = "regression", 
          metric = "rmse",
          max_depth = hyper_grid$max_depth[j],
          num_leaves = hyper_grid$num_leaves[j],
          num_iterations = hyper_grid$num_iterations[j],
          early_stopping = hyper_grid$early_stopping_rounds[j],
          learning_rate = hyper_grid$learning_rate[j], 
          verbose = 1
        ), 
        nfold = 7L,
        data = train_lgb
      )
      
      print(lgb.train)

      # features used
      feature_imp <- lgb.importance(final_model, percentage = TRUE)
      feature_imp$cv_split <- i
      feature_imp <- cbind(feature_imp, hyper_grid[j,])
      
      cv_feature_importances <- rbind(cv_feature_importances, feature_imp)
      
      train_preds <- as.data.table(cbind(unit_sales = tr_cv$unit_sales, 
                                         unit_sales_pred=predict(light_gbn_tuned, as.matrix(tr_cv[, ..pred_features])),
                                         weights = ((tr_cv$perishable * .25) + 1)))
      get_error_metrics(train_preds$unit_sales, train_preds$unit_sales_pred, train_preds$weights)
      
      test_preds <- as.data.table(cbind(unit_sales = te_cv$unit_sales, 
                                         unit_sales_pred=predict(light_gbn_tuned, as.matrix(te_cv[, ..pred_features])),
                                         weights = ((te_cv$perishable * .25) + 1)))
      get_error_metrics(test_preds$unit_sales, test_preds$unit_sales_pred, test_preds$weights)
      
      
    
    
    }
    
  }
  
}



