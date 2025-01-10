# libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot) # correlation plot
library(lubridate) # split hours, days and months
library(DMwR2)
library(VIM)
library(caret)
library(car)
library(forecast) # ARIMA model
library(tseries)

# Load the dataset
airquality <- read.csv('AirQualityUCI.csv')

#### EDA ####
# info about the dataset
head(airquality)
str(airquality)
summary(airquality)

# replace the missing values -200 with NA
airquality[airquality == -200 ] <- NA

# check the missing values
colSums(is.na(airquality))

# Convert Date and Time columns to a single datetime column
airquality$Date <- as.POSIXct(paste(airquality$Date, airquality$Time), format="%d/%m/%Y %H:%M:%S")

# Create month, day and hour columns
airquality <- airquality %>%
  mutate(hour = factor(hour(Date)),
         weekdays = wday(Date, label = TRUE, week_start = 1), # set start day for monday
         month = factor(month(Date, label = TRUE, abbr = TRUE), levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")))


# Count rows with missing values for each month
rows_with_missing_by_month <- airquality %>%
  group_by(month) %>%
  summarise(rows_with_missing = sum(rowSums(is.na(across(everything()))) > 0)) %>%
  arrange(month)


rows_with_missing_by_month
# Plot histograms for numerical variables
airquality %>% select_if(is.numeric) %>%
  gather() %>% 
  ggplot(aes(value)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  facet_wrap(~ key, scales = "free_x")

# Boxplots for numerical variables
airquality %>% select_if(is.numeric) %>%
  pivot_longer(everything()) %>% 
  ggplot(aes(x = name, y = value)) +  
  geom_boxplot(fill = "skyblue", color = "black") + 
  theme_minimal() +
  xlab("Variables") +
  ylab("Values") +
  ggtitle("Boxplots of Numeric Variables in airquality Dataset") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#### Outlierss #### 
# Define the variable names for which you want to identify outliers
outlier_variables <- c("PT08_S1_CO", "NMHC_GT", "PT08_S2_NMHC", "NOx_GT", "PT08_S3_NOx", "NO2_GT", "PT08_S4_NO2", "PT08_S5_O3")

# Create a long format dataframe for easier manipulation
airquality_long <- airquality %>%
  pivot_longer(cols = all_of(outlier_variables), names_to = "variable", values_to = "value")

# Calculate Q1, Q3, and IQR for each variable and mark outliers
airquality_outliers <- airquality_long %>%
  group_by(variable) %>%
  mutate(
    Q1 = quantile(value, 0.25, na.rm = TRUE),
    Q3 = quantile(value, 0.75, na.rm = TRUE),
    IQR_value = Q3 - Q1,
    is_outlier = ifelse(value > (Q3 + 1.5 * IQR_value) | value < (Q1 - 1.5 * IQR_value), "Outlier", "Normal")
  )

# Plotting the outliers for each variable over time
ggplot(airquality_outliers, aes(x = Date, y = value, color = is_outlier)) +
  geom_point() +
  facet_wrap(~ variable, scales = "free_y") +  # Create a separate plot for each variable
  theme_minimal() +
  ggtitle("Outlier Indication for Selected Variables Over Time") +
  xlab("Date") +
  ylab("Value")


#### Data Cleaning ####
# Remove the Time column
airquality$Time <- NULL

# Remove the NMHC_GT (8443 missing values)
airquality$NMHC_GT <- NULL

# Remove the rows if there are more then 2 missing values
airquality <- airquality[rowSums(is.na(airquality)) < 2, ]
colSums(is.na(airquality))
# CO_GT= 452 , NO2_GT = 3

str(airquality)

#### KNN Imputaton ####
airquality_knn <- kNN(airquality, k = 5)
airquality_knn <- dplyr::select(airquality_knn, -ends_with(".imp"))

# remove created colums during imputation!
airquality_knn <- airquality_knn %>% 
  dplyr::select(-ends_with("_imp"))


# check new dataset
colSums(is.na(airquality_knn))
str(airquality_knn)
head(airquality_knn)
summary(airquality_knn)

# Plot histograms for numerical variables
airquality_knn %>% select_if(is.numeric) %>%
  gather() %>% 
  ggplot(aes(value)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  facet_wrap(~ key, scales = "free_x")

# Correlation matrix 
# Removed date, hour, day, month columns not numeric
cor_matrix <- cor(airquality_knn[, c(2:13)], use = "complete.obs")
corrplot::corrplot(cor_matrix, method = 'square', type = 'full', insig='blank',
         addCoef.col ='black', number.cex = 0.8, diag=FALSE)

#c(3, 6:9, 11:13, 4) removed high correlated variables as well for our modeling
cor_matrix_clean <- cor(airquality_knn[, c(3, 6:9, 11:13, 4)], use = "complete.obs")
corrplot::corrplot(cor_matrix_clean, method = 'square', type = 'full', insig='blank',
                   addCoef.col ='black', number.cex = 0.8, diag=FALSE)

#### Time series Visaulisation ####
# Time series plot for Benzene(C6H6_GT)
ggplot(airquality_knn, aes(x = Date, y = C6H6_GT)) +
  geom_line(colour = 'navy') +
  labs(title = "Time Series Plots of Benzene", x = "Time", y = "C6H6_GT (microg/m^3)") +
  theme_minimal()

# Time series plot for CO (GT)
ggplot(airquality_knn, aes(x = Date, y = CO_GT)) +
  geom_line(color = "blue") +
  labs(title = "Time Series Plot of CO (GT)", x = "Time", y = "CO (mg/m^3)") +
  theme_minimal()


# Hourly variation plot for benzene
daily_plot <- airquality_knn %>% 
  group_by(hour) %>% 
  summarise(mean_C6H6 = mean(C6H6_GT),
            sd_C6H6 = sd(C6H6_GT)) %>% 
  ggplot(aes(x = as.numeric(as.character(hour)), y = mean_C6H6)) +
  geom_line(aes(group = 1), color = 'blue') +
  geom_point(size = 3, shape = 15, color = "blue") +
  geom_ribbon(aes(ymin = mean_C6H6 - sd_C6H6, ymax = mean_C6H6 + sd_C6H6), alpha = 0.7, fill = "pink") +
  labs(x = "Hour", y = "C6H6_GT (microg/m^3)", title = "Hourly Variation") +
  theme_minimal()

daily_plot

# Weekly variation plot for benzene
weekly_plot <- airquality_knn %>%
  group_by(weekdays, hour) %>%
  summarise(mean_C6H6 = mean(C6H6_GT),
            sd_C6H6 = sd(C6H6_GT))  %>%
  ggplot(aes(x = as.numeric(as.character(hour)), y = mean_C6H6)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = mean_C6H6 - sd_C6H6, ymax = mean_C6H6 + sd_C6H6),  alpha = 0.7, fill = "pink") +
  facet_wrap(~weekdays, nrow = 1, labeller = labeller(weekdays = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))) +
  scale_x_continuous(breaks = c(0, 6, 12, 18, 23)) +
  labs(x = "Hour", y = "C6H6_GT (microg/m^3)", title = "Weekly Variation") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.spacing.x = unit(0, "lines"))

weekly_plot

# Monthly variation plot for benzene
monthly_plot <- airquality_knn %>% 
  group_by(month) %>% 
  summarise(mean_C6H6 = mean(C6H6_GT),
            sd_C6H6 = sd(C6H6_GT)) %>% 
  ggplot(aes(x = month, y = mean_C6H6, group = 1)) +
  geom_line(color = 'blue') +
  geom_point(size = 3, shape = 15, color = "blue") +
  geom_ribbon(aes(ymin = mean_C6H6 - sd_C6H6, ymax = mean_C6H6 + sd_C6H6), alpha = 0.7, fill = "pink") +
  labs(x = "Month", y = "C6H6", title = "Monthly Variation") +
  theme_minimal()

monthly_plot


### Log Transformations for C6H6_GT, PT08_S3_NOx, and NOx_GT ###
airquality_transformed <- airquality_knn
airquality_transformed$C6H6_GT <- log(airquality_knn$C6H6_GT + 1)       
airquality_transformed$PT08_S3_NOx <- log(airquality_knn$PT08_S3_NOx + 1) 
airquality_transformed$NOx_GT <- log(airquality_knn$NOx_GT + 1)



# Drop the columns 'month', 'hour', and 'weekdays' if they exist and highle correlated variables 
airquality_transformed <- airquality_transformed[, !names(airquality_transformed) %in% c("CO_GT", "PT08_S2_NMHC", "T", "Date", "month", "hour", "weekdays")]

#### Linear Model ####
# Split the train and test split: 70% for training, 30% for testing
train_size <- floor(0.7 * nrow(airquality_transformed))

# Train and test split (use log-transformed target variable)
train_data <- airquality_transformed[1:train_size, ]
test_data <- airquality_transformed[(train_size + 1):nrow(airquality_transformed), ]

train_y <- train_data$C6H6_GT  # Target variable for training
train_data <- train_data[, !(names(train_data) %in% c("C6H6_GT"))]  # Training features

# Fit a linear regression model using the training set
linear_model <- lm(train_y ~ ., data = train_data)
summary(linear_model)
vif(linear_model)

# Create a sequence index for plotting
index <- 1:nrow(airquality_transformed)

# Extract training and testing data for visualization
train_y_full <- airquality_transformed$C6H6_GT[1:train_size]  # Training data (C6H6_GT)
test_y_full <- airquality_transformed$C6H6_GT[(train_size + 1):nrow(airquality_transformed)]  # Testing data (C6H6_GT)

#Train-Test Split Visualization
plot(index, airquality_transformed$C6H6_GT, type = "n",
     main = "Train-Test Split Visualization for C6H6_GT",
     xlab = "Time Index",
     ylab = "C6H6_GT (Log-Transformed)")

# Training set blue line
lines(index[1:train_size], train_y_full, col = "blue", lwd = 2)
# Test set red line
lines(index[(train_size + 1):nrow(airquality_transformed)], test_y_full, col = "red", lwd = 2)
legend("topright", legend = c("Training Set", "Test Set"),
       col = c("blue", "red"), lty = 1, lwd = 2)



### Predictions and Back Transformation ###
# Prepare test data
test_y <- test_data$C6H6_GT  # Target variable for testing
test_data <- test_data[, !(names(test_data) %in% c("C6H6_GT"))]  # Testing features

# Make predictions on the test set
y_pred_linear <- predict(linear_model, newdata = test_data)

### Residual Analysis (on Log-Transformed Scale) ###
residuals_log <- test_y - y_pred_linear  # Residuals on log scale

# Plot Residuals vs Fitted (Log Scale)
plot(y_pred_linear, residuals_log,
     main = "Residuals vs Fitted (Log Scale)",
     xlab = "Fitted Values (Log Scale)",
     ylab = "Residuals (Log Scale)")
abline(h = 0, col = "red")

# Check normality of residuals using Q-Q plot (Log Scale)
qqnorm(residuals_log, main = "Q-Q Plot of Residuals (Log Scale)")
qqline(residuals_log, col = "red")

# Back-transform predictions to the original scale
y_pred_linear_backtransformed <- exp(y_pred_linear) - 1
test_y_backtransformed <- exp(test_y) - 1

# Define MAE, MSE, and RMSE functions
mae <- function(actual, predicted) {
  mean(abs(actual - predicted))
}
mse <- function(actual, predicted) {
  mean((actual - predicted)^2)
}
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# Calculate evaluation metrics on the original scale
mae_value <- mae(test_y_backtransformed, y_pred_linear_backtransformed)
mse_value <- mse(test_y_backtransformed, y_pred_linear_backtransformed)
rmse_value <- rmse(test_y_backtransformed, y_pred_linear_backtransformed)

# Print results
print(paste("MAE (original scale):", mae_value))
print(paste("MSE (original scale):", mse_value))
print(paste("RMSE (original scale):", rmse_value))


### Cross Validation ####
#### Perform expending window Cross-Validation for linear model ####
cv_linear_model <- function(data, target, folds) {
  n <- nrow(data)
  fold_size <- floor(n / folds)
  
  mae_values <- c()
  mse_values <- c()
  rmse_values <- c()
  
  all_residuals_log <- c()
  all_fitted_values <- c()
  
  for (i in 1:folds) {
    # Define expanding window for training and the next segment for testing
    train_index <- 1:(fold_size * i)
    test_index <- (fold_size * i + 1):min(n, fold_size * (i + 1))
    
    # Only proceed if there’s enough data left for a test set
    if (length(test_index) < fold_size) break

    train_data <- data[train_index, ]
    train_y <- target[train_index]
    test_data <- data[test_index, ]
    test_y <- target[test_index]
    
    cv_linear <- lm(train_y ~ ., data = train_data)
    
    # Print model summary for the current fold
    cat("\n\n--- Model Summary for Fold", i, "---\n")
    print(summary(cv_linear))
    print(vif(cv_linear))
   
    
    # Print the size of training and test sets for each fold
    cat("\nFold", i, "Training set size:", length(train_index), "Test set size:", length(test_index), "\n")
    
    # Predictions and metrics as before
    y_pred_linear <- predict(linear_model, newdata = test_data)
    y_pred_linear_backtransformed <- exp(y_pred_linear) - 1
    test_y_backtransformed <- exp(test_y) - 1
    
    # Calculate evaluation metrics for each fold
    mae_value <- mean(abs(test_y_backtransformed - y_pred_linear_backtransformed))
    mse_value <- mean((test_y_backtransformed - y_pred_linear_backtransformed)^2)
    rmse_value <- sqrt(mse_value)
    
    # Store metrics
    mae_values <- c(mae_values, mae_value)
    mse_values <- c(mse_values, mse_value)
    rmse_values <- c(rmse_values, rmse_value)
    
    # Calculate residuals on log-transformed scale and fitted values
    fold_residuals_log <- test_y - y_pred_linear
    fold_fitted_values <- y_pred_linear
    
    # Append residuals and fitted values from this fold to the overall vectors
    all_residuals_log <- c(all_residuals_log, fold_residuals_log)
    all_fitted_values <- c(all_fitted_values, fold_fitted_values)
  }
  # Calculate average metrics across all folds
  mae_avg <- mean(mae_values)
  mse_avg <- mean(mse_values)
  rmse_avg <- mean(rmse_values)
  
  # Print the cross-validation results
  cat("\n\n--- Cross-Validation Results ---\n")
  print(paste("Average MAE (original scale):", mae_avg))
  print(paste("Average MSE (original scale):", mse_avg))
  print(paste("Average RMSE (original scale):", rmse_avg))
  
  # Plot Residuals vs Fitted (Log Scale)
  plot(all_fitted_values, all_residuals_log,
       main = "Residuals vs Fitted (Cross-Validation, Log Scale)",
       xlab = "Fitted Values (Log Scale)",
       ylab = "Residuals (Log Scale)")
  abline(h = 0, col = "red")
  
  # Q-Q Plot of residuals (Log Scale)
  qqnorm(all_residuals_log, main = "Q-Q Plot of Residuals (Cross-Validation, Log Scale)")
  qqline(all_residuals_log, col = "red")
  
  # Return average metrics as a list
  return(list(mae = mae_avg, mse = mse_avg, rmse = rmse_avg))
}

# Prepare dataset and target variable
data <- airquality_transformed[, !(names(airquality_transformed) %in% c("C6H6_GT"))]
target <- airquality_transformed$C6H6_GT

# Set number of folds
folds <- 10

# Perform cross-validation
cv_results <- cv_linear_model(data, target, folds)

mae_cv_value <- 1.47228885064888  # Replace with actual MAE from the linear model
mse_cv_value <- 7.04527284775303  # Replace with actual MSE from the linear model
rmse_cv_value <- 2.49438440440703

# Plot setup
n <- nrow(data)  # Total number of data points
fold_size <- floor(n / folds)  # Size of each test set

plot(1, type = "n", xlim = c(1, n), ylim = c(1, folds), 
     xlab = "Data Index", ylab = "Fold", 
     main = "Training and Test Set Splits for Each Fold (Expanding Window)")

for (i in 1:folds) {
  train_index <- 1:(fold_size * i)
  test_index <- (fold_size * i + 1):min(n, fold_size * (i + 1))
  
  if (length(test_index) < fold_size) break
  
  # Training set in blue
  segments(x0 = train_index, y0 = rep(i, length(train_index)), 
           x1 = train_index, y1 = rep(i, length(train_index)), 
           col = "blue", lwd = 2)
  
  # Test set in red
  segments(x0 = test_index, y0 = rep(i, length(test_index)), 
           x1 = test_index, y1 = rep(i, length(test_index)), 
           col = "red", lwd = 2)
}

legend("bottomright", legend = c("Training Set", "Test Set"), 
       col = c("blue", "red"), lty = 1, lwd = 2)

########################################

# Create a data frame to hold the comparison metrics for both models

comparison_table <- data.frame(
  Metric = c(
    "MAE",
    "MSE",
    "RMSE"
  ),
  Linear_model = c(
    mae_value,
    mse_value,
    rmse_value  
),
  Cross_Validation_Model = c(
    mae_cv_value, 
    mse_cv_value,
    rmse_cv_value
  )
)

write.csv(comparison_table, file = "model_comparison.csv", row.names = FALSE)
# Print the comparison table
print(comparison_table)
########################################

### Decomposition ####

# Change frequency depends on time period, hourly data = 24, weekly = 168, monthly = 720
benzene_ts <- ts(airquality_knn$C6H6_GT, frequency = 168) 
decomposed_benzene <- stl(benzene_ts, s.window = "periodic", robust = T)
plot(decomposed_benzene, main = "Decomposition of Benzene Concentration Time Series")

#######ARIMAX##########
library(forecast)

# Define the response variable
benzene_log <- log(airquality_transformed$C6H6_GT + 1)

# Define the exogenous variables matrix
exogenous_vars <- as.matrix(airquality_transformed[, c("PT08_S1_CO", "NOx_GT", "PT08_S4_NO2", "PT08_S3_NOx")])

# Split data into training and test sets
n <- length(benzene_log)
train_size <- round(n * 0.75)  # 75% for training
test_size <- n - train_size    # 25% for testing

# Training and testing data
train_data <- benzene_log[1:train_size]
train_exog <- exogenous_vars[1:train_size, ]
test_exog <- exogenous_vars[(train_size + 1):n, ]

# Fit the ARIMAX model on the training data
arimax_model <- auto.arima(train_data, xreg = train_exog)

# Summary of the fitted model
summary(arimax_model)

# Forecast for the test period using the ARIMAX model
forecast_values <- forecast(arimax_model, xreg = test_exog, h = test_size)

# Plot the forecasted vs actual values
plot(forecast_values, main = "ARIMAX Model Forecast", ylab = "Log Benzene Concentration")
lines((train_size + 1):n, benzene_log[(train_size + 1):n], col = "black")  # Add actual values for comparison
legend("topright", legend = c("Forecast", "Train"), col = c("blue", "black"), lty = 1)


# Assuming 'arimax_model' is the fitted ARIMAX model
checkresiduals(arimax_model)

# Ljung-Box test
Box.test(arimax_model$residuals, lag = 20, type = "Ljung-Box")

# Back-transform forecasted and actual values to the original scale
forecast_values_original <- exp(forecast_values$mean) - 1
actual_values_original <- exp(benzene_log[(train_size + 1):n]) - 1

# Calculate error metrics on the original scale
error_metrics_original <- data.frame(
  MAE = mean(abs(forecast_values_original - actual_values_original)),
  MSE = mean((forecast_values_original - actual_values_original)^2),
  RMSE = sqrt(mean((forecast_values_original - actual_values_original)^2))
)

# Print the error metrics
print(error_metrics_original)

