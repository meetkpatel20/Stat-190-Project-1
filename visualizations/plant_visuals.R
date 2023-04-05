library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(pROC)
library(tidytext)
library(topicmodels)
library(reshape2)
library(randomForest)

full_data <- read.csv('FullWithNulls.csv', header=T)

full_data <- full_data[,-c(1)] # bye bye random X column

# subsetting columns to only grab our plant fault code and expl vars
plant_data <- full_data[, c(2,4,5,6,7,8,9,10,232)]

print(plant_data[plant_data$ErrorType_Balance.of.Plant > 0, c('interval_time')])
error_interval1 <- interval(ymd("2022-02-01"), ymd_hms("2022-02-03 15:40:00"))
plant_data$interval_time = ymd_hms(plant_data$interval_time)
plant_subset1 <- plant_data[plant_data$interval_time %within% error_interval1,]


ggplot(plant_subset1, aes(x=interval_time)) + 
  geom_point(aes(y = IMS.Bearing.Temp), color = "darkred", size=1) + 
  labs(title="Mean Bearing Temp Leading up to a Plant Error", x="Date", y="Temp (Celcius)")

ggplot(plant_subset1, aes(x=interval_time)) + 
  geom_point(aes(y = mean_wind), color = "darkblue", size=1) + 
  labs(title="Mean Windspeed Leading up to a Plant Error", x="Date", y="MPH")

ggplot(plant_subset1, aes(x=interval_time)) + 
  geom_point(aes(y = Oil.Temp), color = "orange", size=1) + 
  labs(title="Mean Oil Temp Leading up to a Plant Error", x="Date", y="Temp (Celcius)")

ggplot(plant_subset1, aes(x=interval_time)) + 
  geom_point(aes(y = psi), color = "darkgreen", size=1) + 
  labs(title="Mean Hydraulic Pressure Leading up to a Plant Error", x="Date", y="psi")

ggplot(plant_subset1, aes(x=interval_time)) + 
  geom_point(aes(y = kilowatts), color = "purple", size=1) + 
  labs(title="Mean Active Power Leading up to a Plant Error", x="Date", y="kilowatts")

ggplot(plant_subset1, aes(x=interval_time)) + 
  geom_point(aes(y = mean_RepPerMinute), color = "magenta", size=1) + 
  labs(title="Mean Reps per Minute Leading up to a Plant Error", x="Date", y="kilowatts")
