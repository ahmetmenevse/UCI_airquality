# libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot) # correlation plot
library(lubridate) # split hours, days and months
library(DMwR2)
library(VIM)
library(caret)

# Load the dataset
airquality <- read.csv('../Datasets/AirQualityUCI.csv')

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
  summarise(rows_with_missing = sum(if_any(everything(), is.na))) %>%
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
# Removed date, hour, day, month columns
cor_matrix <- cor(airquality_knn[2:13], use = "complete.obs")
corrplot::corrplot(cor_matrix, method = 'square', type = 'full', insig='blank',
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


### Decomposition ####
# Change frequency depends on time period, hourly data = 24, weekly = 168, monthly = 720
benzene_ts <- ts(airquality_knn$C6H6_GT, frequency = 168) 
decomposed_benzene <- stl(benzene_ts, s.window = "periodic", robust = T)
plot(decomposed_benzene)

# Apply log transformation to the target variable
log_benzene_ts <- log(airquality_knn$C6H6_GT + 1)

# Embed the log-transformed time series with weekly (168-hour) lag
data_matrix <- as.matrix(embed(log_benzene_ts, 168)) 
colnames(data_matrix) <- paste0("Lag", 1:168)
data_matrix <- as.data.frame(data_matrix)

#### PCA ####
# PCA for log-transformed benzene
temp_pca <- prcomp(data_matrix, center = TRUE, scale. = TRUE)
summary(temp_pca)

# Extract first 21 principal components to retain ~80% variability(PC17 and PC20 removed due to not statisticly significant)
pca_data <- as.data.frame(temp_pca$x[, c(1:16, 18, 21)])

# Number of lags in PCA embedding (168)
lag_num <- 168

#### Linear Regression
# Define the size of train and test set for the features after embedding
train_size <- floor(0.7 * nrow(pca_data))  # 70%  training
test_size <- nrow(pca_data) - train_size

# Split the PCA-transformed data into train and test sets
pca_train <- pca_data[1:train_size, ]
pca_test <- pca_data[(train_size + 1):nrow(pca_data), ]

# Split the original log-transformed target variable accordingly, with adjustment for the lag
# The first 168 observations cannot be used because of embedding, adjust indexing accordingly
y_train <- log_benzene_ts[(lag_num + 1):(lag_num + train_size)]
y_test <- log_benzene_ts[(lag_num + train_size + 1):(lag_num + train_size + test_size)]

# Fit a linear regression model using the training set
pca_train$y_train <- y_train  # Include y_train in training data
linear_model <- lm(y_train ~ ., data = pca_train)
summary(linear_model)

# Make predictions and back-transform
y_pred_linear <- predict(linear_model, newdata = pca_test)
y_pred_linear_backtransformed <- exp(y_pred_linear) - 1


valid_indices <- which(!is.na(y_test) & !is.na(y_pred_linear))
y_test_valid <- y_test[valid_indices]
y_pred_linear_valid <- y_pred_linear[valid_indices]

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

# Calculate RMSE on log scale and back-transformed scale
rmse_linear <- rmse(y_test_valid, y_pred_linear_valid)
print(paste("Linear Model RMSE (log scale):", rmse_linear))

# Back-transform and calculate RMSE
y_test_valid_backtransformed <- exp(y_test_valid) - 1
y_pred_linear_backtransformed <- y_pred_linear_backtransformed[valid_indices]
rmse_linear_backtransformed <- rmse(y_test_valid_backtransformed, y_pred_linear_backtransformed)
print(paste("Linear Model RMSE (original scale):", rmse_linear_backtransformed))

# Calculate MAE on log scale
mae_linear <- mae(y_test_valid, y_pred_linear_valid)
print(paste("Linear Model MAE (log scale):", mae_linear))

# Back-transform and calculate MAE
mae_linear_backtransformed <- mae(y_test_valid_backtransformed, y_pred_linear_backtransformed)
print(paste("Linear Model MAE (original scale):", mae_linear_backtransformed))

# Plot residuals vs fitted values for linear model!
plot(y_pred_linear_valid, residuals(linear_model)[valid_indices], main = "Residuals vs Fitted", xlab = "Fitted values", ylab = "Residuals")
abline(h = 0, col = "red")

## R-squared (R²) and Adjusted R-squared
r_squared <- summary(linear_model)$r.squared
adjusted_r_squared <- summary(linear_model)$adj.r.squared
print(paste("Linear Model R-squared:", r_squared))
print(paste("Linear Model Adjusted R-squared:", adjusted_r_squared))


# Calculate MSE on log scale
mse_linear <- mse(y_test_valid, y_pred_linear_valid)
print(paste("Linear Model MSE (log scale):", mse_linear))

# Back-transform and calculate MSE
mse_linear_backtransformed <- mse(y_test_valid_backtransformed, y_pred_linear_backtransformed)
print(paste("Linear Model MSE (original scale):", mse_linear_backtransformed))

# Check normality of residuals
qqnorm(residuals(linear_model)[valid_indices])
qqline(residuals(linear_model)[valid_indices], col = "red")

# Plot Predicted vs Actual for back-transformed values
plot(y_test_valid_backtransformed, y_pred_linear_backtransformed, main = "Predicted vs Actual (Original Scale)", xlab = "Actual Values", ylab = "Predicted Values")
abline(0, 1, col = "blue")

# Plot Train vs Test Data
plot(seq_along(y_train), y_train, type = "l", col = "blue", lwd = 2, xlab = "Time Index", ylab = "C6H6_GT (Log-Transformed)", main = "Train and Test Data")
lines((length(y_train) + 1):(length(y_train) + length(y_test)), y_test, col = "red", lwd = 2)
legend("topleft", legend = c("Train Data", "Test Data"), col = c("blue", "red"), lty = 1, lwd = 2)


#### Perform Cross-Validation for linear model ####
# Rolling Time-Slice Cross-Validation
set.seed(123)
train_control <- trainControl(method = "timeslice",
                              initialWindow = 0.7 * length(y_train),
                              horizon = 168, 
                              fixedWindow = TRUE,
                              savePredictions = "all")
cross_val_model <- train(y_train ~ ., data = pca_train, method = "lm", trControl = train_control)
summary(cross_val_model)

# Extract observed and predicted values from cross-validation
y_actual_log <- cross_val_model$pred$obs        # Observed values on log-transformed scale
y_pred_log <- cross_val_model$pred$pred         # Predicted values on log-transformed scale

# Back-transform to original scale
y_actual_original <- exp(y_actual_log) - 1
y_pred_original <- exp(y_pred_log) - 1

# MAE on log-transformed scale
mae_log <- mae(y_actual_log, y_pred_log)
print(paste("Cross-Validation Model MAE (Log Scale):", mae_log))

# MAE on original scale
mae_original <- mae(y_actual_original, y_pred_original)
print(paste("Cross-Validation Model MAE (Original Scale):", mae_original))

# MSE on log-transformed scale
mse_log <- mse(y_actual_log, y_pred_log)
print(paste("Cross-Validation Model MSE (Log Scale):", mse_log))

# MSE on original scale
mse_original <- mse(y_actual_original, y_pred_original)
print(paste("Cross-Validation Model MSE (Original Scale):", mse_original))

# RMSE on log-transformed scale
rmse_log <- rmse(y_actual_log, y_pred_log)
print(paste("Cross-Validation Model RMSE (Log Scale):", rmse_log))

# RMSE on original scale
rmse_original <- rmse(y_actual_original, y_pred_original)
print(paste("Cross-Validation Model RMSE (Original Scale):", rmse_original))


# Calculate R-squared on log-transformed scale
r_squared_log <- cor(y_actual_log, y_pred_log)^2
print(paste("Cross-Validation Model R-squared (Log Scale):", r_squared_log))

# Calculate Adjusted R-squared on log-transformed scale
n <- length(y_actual_log)  # Number of observations
p <- length(cross_val_model$finalModel$coefficients) - 1  # Number of predictors (excluding intercept)
adjusted_r_squared_log <- 1 - ((1 - r_squared_log) * (n - 1) / (n - p - 1))
print(paste("Cross-Validation Model Adjusted R-squared (Log Scale):", adjusted_r_squared_log))

# Calculate R-squared on original scale
r_squared_original <- cor(y_actual_original, y_pred_original)^2
print(paste("Cross-Validation Model R-squared (Original Scale):", r_squared_original))

# Calculate Adjusted R-squared on original scale
adjusted_r_squared_original <- 1 - ((1 - r_squared_original) * (n - 1) / (n - p - 1))
print(paste("Cross-Validation Model Adjusted R-squared (Original Scale):", adjusted_r_squared_original))

# Calculate residuals on log-transformed scale
residuals_log <- y_actual_log - y_pred_log

# Residuals vs Fitted values (Log Scale)
plot(y_pred_log, residuals_log, main = "Residuals vs Fitted (Cross-Validation, Log Scale)",
     xlab = "Fitted Values (Log Scale)", ylab = "Residuals (Log Scale)")
abline(h = 0, col = "red")

# QQ Plot for residuals on log-transformed scale
qqnorm(residuals_log, main = "QQ Plot of Residuals (Cross-Validation, Log Scale)")
qqline(residuals_log, col = "red")

#### Train and test set visualise
# Updated Train-Test Split Visualization for Rolling Time-Slice Cross-Validation
set.seed(123)
initial_window <- 0.7 * length(y_train)  # test set (70%)
horizon <- 168  # test weekly (168 hour)
increment <- 24  # continue next day

# Rolling Time-Slice Cross-Validation calculate the fold (60)
n_folds <- floor((length(y_train) - initial_window) / increment)

# plot for test and train set
plot(1:length(y_train), rep(NA, length(y_train)), type = "n",
     ylim = c(0, n_folds + 1), xlab = "Index", ylab = "Fold", 
     main = "Rolling Time-Slice Cross-Validation Train-Test Splits")

# visaualise every fold
for (i in 1:n_folds) {
  # start and end frame
  train_start <- 1 + (i - 1) * increment
  train_end <- train_start + initial_window - 1
  test_start <- train_end + 1
  test_end <- min(test_start + horizon - 1, length(y_train))
  rect(train_start, i - 0.4, train_end, i, col = "skyblue", border = NA)
  rect(test_start, i - 0.4, test_end, i, col = "lightcoral", border = NA)
}

# Legend 
legend("topleft", legend = c("Training Set", "Test Set"), 
       fill = c("skyblue", "lightcoral"), border = NA)

########################################

