library(ggplot2)
library(Metrics)

#"It avoids penalizing large differences in prediction when both the predicted and the true number are large
#predicting 5 when the true value is 50 is penalized more than predicting 500 when the true value is 545."
nrmse <- function(y, pred, weights = rep(1, length(y))) {
  y <- pmax(0, pmin(y, max(y, na.rm = TRUE)))
  pred <- pmax(0, pmin(pred, max(pred, na.rm = TRUE)))
  score <- sum(weights * ((log(pred + 1) - log(y + 1)) ^ 2), na.rm = TRUE) / sum(weights, na.rm = TRUE)
  return(sqrt(score))
}

calc_errors <- function(actual, predicted) {
  
  diff = (predicted - actual)
  abs_diff = abs(predicted - actual)
  
  diff_perc = ifelse(actual < predicted, -((actual/predicted) - 1), (predicted/actual) - 1)
  
  
  nrmse <- nrmse(actual, predicted)
  rmse <- Metrics::rmse(actual, predicted)
  
  em_df <- data.frame(actual = actual,
                      predicted = predicted,
                      diff = diff, 
                      abs_diff = abs_diff,
                      diff_perc = diff_perc,
                      rmse = rmse,
                      nrmse = nrmse)
  
  return(em_df)
  
}


# Generate some example data
#mean(tr$unit_sales) = 8.5
#sd(tr$unit_sales) = 23

set.seed(43)
actual <- abs(rnorm(10000, 8.5, 23))
predicted <- abs(actual + rnorm(10000, 0, 23))

error_metrics = list()
for(i in 1:length(actual)){
  
  error_metrics[[i]] = calc_errors(actual[i], predicted[i])
  
}
df <- do.call(rbind, error_metrics)
df <- df[df$diff >= -60 & df$diff <= 60,]
df$rmse_scaled <- df$rmse / 20


ggplot(df, aes(x = diff)) + 
  geom_point(aes(y = rmse_scaled, color = "blue")) + 
  geom_point(aes(y = nrmse, color = "red")) +
  scale_color_discrete(labels = c("RMSE(/20)", "NRMSE")) +
  labs(title = "RMSE and NRMSE vs. (Predicted - Actual)", 
       x = "(Predicted - Actual)",
       y = "error value")


ggplot(df, aes(x = diff_perc)) + 
  geom_point(aes(y = rmse_scaled, color = "blue")) + 
  geom_point(aes(y = nrmse, color = "red")) +
  scale_color_discrete(labels = c("RMSE(/20)", "NRMSE")) +
  labs(title = "RMSE and NRMSE vs. % Difference Predicted and Actual", 
       x = "% Difference Predicted and Actual",
       y = "error value")


set.seed(123)
actual <- rep(10, 1000)
predicted <- seq(0, 40, length.out = 1000)

error_metrics = list()
for(i in 1:length(actual)){
  
  error_metrics[[i]] = calc_errors(actual[i], predicted[i])
  
}
df_fixed_actual <- do.call(rbind, error_metrics)
df_fixed_actual$rmse_scaled <- df_fixed_actual$rmse / 20

######
ggplot(df_fixed_actual, aes(x = diff)) + 
  geom_point(aes(y = rmse_scaled, color = "blue")) + 
  geom_point(aes(y = nrmse, color = "red")) +
  scale_color_discrete(labels = c("RMSE(/20)", "NRMSE")) +
  labs(title = "RMSE and NRMSE vs. (Predicted - Actual)", 
       x = "(Predicted - Actual)",
       y = "error value")


####
ggplot(df_fixed_actual, aes(x = diff_perc)) + 
  geom_point(aes(y = rmse_scaled, color = "blue")) + 
  geom_point(aes(y = nrmse, color = "red")) +
  scale_color_discrete(labels = c("RMSE(/20)", "NRMSE")) +
  labs(title = "RMSE and NRMSE vs. % Difference Predicted and Actual", 
       x = "% Difference Predicted and Actual",
       y = "error value")



