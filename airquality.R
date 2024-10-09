airquality <- read.csv('../Datasets/AirQualityUCI.csv')
str(airquality)

#check missing values!(-200 values are missing)
airquality[airquality == -200 ] <- NA
colSums(is.na(airquality))




