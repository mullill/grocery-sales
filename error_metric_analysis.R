# Create plot
library(ggplot2)
library(Metrics)

# Generate some example data
#mean(tr$unit_sales) = 3.98276
#sd(tr$unit_sales) = 18.15048

set.seed(123)
actual <- abs(rnorm(1000, 4, 18))
predicted <- abs(actual + rnorm(1000, 0, 18))

calc_errors <- function(row) {
  actual <- abs(row[1])
  predicted <- abs(row[2])
  
  diff = (predicted - actual)
  abs_diff = abs(predicted - actual)
  diff_perc = predicted/actual
  
  nrmse <- nrmse(actual, predicted)
  rmse <- Metrics::rmse(actual, predicted)
  
  names <- c("diff", "abs_diff", "diff_perc", "rmse", "nrmse")
  return(setNames(c(diff, abs_diff, diff_perc, rmse, nrmse), names))
}


nrmse <- function(y, pred, weights = rep(1, length(y))) {
  y <- pmax(0, pmin(y, max(y, na.rm = TRUE)))
  pred <- pmax(0, pmin(pred, max(pred, na.rm = TRUE)))
  score <- sum(weights * ((log(pred + 1) - log(y + 1)) ^ 2), na.rm = TRUE) / sum(weights, na.rm = TRUE)
  return(sqrt(score))
}


df <- data.frame(actual, predicted)

# Apply the function to each row of the data.frame
df <- cbind(df, t(apply(df[, 1:2], 1, calc_errors)))

df <- df[df$diff_perc < 500,]



ggplot(df, aes(x = diff, y = rmse)) + 
  geom_point() + 
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "RMSE vs. Difference between Actual and Predicted", 
       x = "Difference between Actual and Predicted", 
       y = "RMSE") +
  theme_bw()

ggplot(df, aes(x = diff, y = nrmse)) + 
  geom_point() + 
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "NRMSE vs. Difference between Actual and Predicted", 
       x = "Difference between Actual and Predicted", 
       y = "NRMSE") +
  theme_bw()


ggplot(df, aes(x = diff_perc, y = rmse)) + 
  geom_point() + 
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "RMSE vs. % Difference between Actual and Predicted", 
       x = "% Difference between Actual and Predicted", 
       y = "RMSE") +
  theme_bw()

ggplot(df, aes(x = diff_perc, y = nrmse)) + 
  geom_point() + 
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "NRMSE vs. % Difference between Actual and Predicted", 
       x = "% Difference between Actual and Predicted", 
       y = "NRMSE") +
  theme_bw()
