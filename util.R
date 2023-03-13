#NWRMSLE = \sqrt{ \frac{\sum_{i=1}^n w_i \left( \ln(\hat{y}i + 1) - \ln(y_i +1)  \right)^2  }{\sum{i=1}^n w_i}} $$
nrmse <- function(actual, predicted) {
  numerator <- sqrt(mean((log(actual + 1) - log(predicted + 1))^2))
  denominator <- sqrt(mean((log(actual + 1) - mean(log(actual + 1)))^2))
  return(numerator/denominator)
}





