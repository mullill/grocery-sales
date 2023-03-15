#NWRMSLE = \sqrt{ \frac{\sum_{i=1}^n w_i \left( \ln(\hat{y}i + 1) - \ln(y_i +1)  \right)^2  }{\sum{i=1}^n w_i}} $$
nrmse <- function(actual, predicted) {
  numerator <- sqrt(mean((log(actual + 1) - log(predicted + 1))^2))
  denominator <- sqrt(mean((log(actual + 1) - mean(log(actual + 1)))^2))
  return(numerator/denominator)
}

get_error_metrics <- function(actual, predicted){
  
  mean_actual <- mean(actual)
  mean_pred <- mean(predicted)
  print(paste0("sanity check:::: Mean Actual: ", round(mean_actual, 2), " - mean_pred: ", round(mean_pred, 2)))
  
  rmse = Metrics::rmse(actual, predicted)
  nrmse = nrmse(actual, predicted)
  mae = Metrics::mae(actual, predicted)
  smape = Metrics::smape(actual, predicted)
  #mape = Metrics::mape(actual, predicted)
  
  # quantiles <- c(0.05, 0.10, 0.50, 0.90, 0.95)
  # ql <- quantileLoss(actual, predicted, quantiles)
  
  return(data.frame(rmse, nrmse, mae, smape))
  
}
  
  





