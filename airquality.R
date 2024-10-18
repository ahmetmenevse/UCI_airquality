# libraries
library(ggplot2)
library(corrplot) # correlation plot
# dataset
airquality <- read.csv('../Datasets/AirQualityUCI.csv', stringsAsFactors = T)

# info about the dataset
str(airquality)
summary(airquality)

# Convert Date and Time columns to a single datetime column
airquality$Date <- as.POSIXct(paste(airquality$Date, airquality$Time), format="%d/%m/%Y %H:%M:%S")

# replace the missing values -200 with NA
airquality[airquality == -200 ] <- NA

# check the missing values
colSums(is.na(airquality))

# Remove the Time column
airquality$Time <- NULL

# Remove the NMHC_GT (8443 missing values)
airquality$NMHC_GT <- NULL

# remove the missing values
clean_airquality <- subset(airquality, complete.cases(airquality))

# check new dataset
colSums(is.na(clean_airquality))
str(clean_airquality)
head(clean_airquality)

## Histograms
# Histogram for CO_GT
ggplot(clean_airquality, aes(x = CO_GT)) +
  geom_histogram(binwidth = 0.3, fill = "skyblue", color = "black") +
  labs(title = "Histogram of CO (GT)", x = "CO(mg/m^3)") +
  theme_minimal()

# Histogram for PT08_S1_CO
ggplot(clean_airquality, aes(x = PT08_S1_CO)) +
  geom_histogram(binwidth = 30, fill = "skyblue", color = "black") +
  labs(title = "Histogram of PT08.S1(CO) ", x = "PT08.S1(CO)") +
  theme_minimal()

# Histogram for C6H6_GT (Benzene)
ggplot(clean_airquality, aes(x = C6H6_GT)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Benzene (C6H6(GT))", x = "Benzene (µg/m^3)") +
  theme_minimal()

# Histogram for PT08_S2_NMHC
ggplot(clean_airquality, aes(x = PT08_S2_NMHC)) +
  geom_histogram(binwidth = 25, fill = "skyblue", color = "black") +
  labs(title = "Histogram of PT08.S2(NMHC) ", x = "PT08.S2(NMHC)") +
  theme_minimal()

# Histogram for NOx_GT
ggplot(clean_airquality, aes(x = NOx_GT)) +
  geom_histogram(binwidth = 25, fill = "skyblue", color = "black") +
  labs(title = "Histogram of NOx (GT)", x = "NOx(GT)") +
  theme_minimal()

# Histogram for PT08_S3_NOx
ggplot(clean_airquality, aes(x = PT08_S3_NOx)) +
  geom_histogram(binwidth = 30, fill = "skyblue", color = "black") +
  labs(title = "Histogram of PT08.S3(NOx) ", x = "PT08.S3(NOx)") +
  theme_minimal()

# Histogram for NO2_GT
ggplot(clean_airquality, aes(x = NO2_GT)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Histogram of NO2 (GT)", x = "NO2 (µg/m^3)") +
  theme_minimal()

# Histogram for PT08_S4_NO2
ggplot(clean_airquality, aes(x = PT08_S4_NO2)) +
  geom_histogram(binwidth = 30, fill = "skyblue", color = "black") +
  labs(title = "Histogram of PT08.S4(NO2) ", x = "PT08.S4(NO2)") +
  theme_minimal()

# Histogram for PT08_S5_O3
ggplot(clean_airquality, aes(x = PT08_S5_O3)) +
  geom_histogram(binwidth = 50, fill = "skyblue", color = "black") +
  labs(title = "Histogram of PT08.S5(O3) ", x = "PT08.S5(O3)") +
  theme_minimal()

# Histogram for Temperature (T)
ggplot(clean_airquality, aes(x = T)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Temperature", x = "Temperature (°C)") +
  theme_minimal()

# Histogram for Relative Humidity
ggplot(clean_airquality, aes(x = RH)) +
  geom_histogram(binwidth = 1.5, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Relative Humidity", x = "Relative Humidity (%)") +
  theme_minimal()

# Histogram for Absolute Humidity
ggplot(clean_airquality, aes(x = AH)) +
  geom_histogram(binwidth = 0.04, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Absolute Humidity", x = "Absolute Humidity") +
  theme_minimal()

# Correlation matrix
cor_matrix <- cor(clean_airquality[, -1], use = "complete.obs")
corrplot(cor_matrix, method = 'square', type = 'full', insig='blank',
         addCoef.col ='black', number.cex = 0.8, diag=FALSE)

###################
# Time series plot for Benzene(C6H6_GT)
ggplot(clean_airquality, aes(x = Date, y = C6H6_GT)) +
  geom_line(colour = 'navy') +
  labs(title = "Time Series Plots of Benzene", x = "Time", y = "C6H6_GT (microg/m^3)") +
  theme_minimal()

# Time series plot for CO (GT)
ggplot(clean_airquality, aes(x = Date, y = CO_GT)) +
  geom_line(color = "blue") +
  labs(title = "Time Series Plot of CO (GT)", x = "Time", y = "CO (mg/m^3)") +
  theme_minimal()

# Decomposition (using stl in R for seasonal decomposition)
# change frequency depends on time period, hourly data = 24, weekly = 168, monthly = 720
benzene_ts <- ts(clean_airquality$C6H6_GT, frequency = 168) 
decomposed_benzene <- stl(benzene_ts, s.window = "periodic", robust = T)
plot(decomposed_benzene)


