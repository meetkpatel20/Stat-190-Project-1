library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)

full_data <- read.csv("FullData2.csv", header=T)

# subset the data to only include explanatory variables and our error code
bypass_data <- full_data[, c(2,3,4,5,6,7,8,9,10,11,222)]

# convert interval_time into a time/date datatype
bypass_data$interval_time <- ymd_hms(bypass_data$interval_time)

# just checking out what dates the error occurred
print(bypass_data[bypass_data$ErrorMessage_Ups.Bypass.Error > 0, c('interval_time')])
# "2022-06-27" "2020-05-06" "2021-02-08" "2021-02-19"

error_interval1 <- interval(ymd("2021-02-15"), ymd_hms("2021-02-17 09:00:00"))
error_interval2 <- interval(ymd("2021-02-13"), ymd_hms("2021-02-17 09:00:00"))
print(error_interval1)

byp_subset1 <- bypass_data[bypass_data$interval_time %within% error_interval1,]
byp_subset2 <- bypass_data[bypass_data$interval_time %within% error_interval2,]


ggplot(byp_subset1, aes(x=interval_time)) + 
  geom_point(aes(y = IMS.Bearing.Temp), color = "darkred") + 
  labs(title="Mean Bearing Temp Leading up to a Bypass Error", x="Date", y="Temp (Celcius)")

ggplot(byp_subset2, aes(x=interval_time)) + 
  geom_point(aes(y = IMS.Bearing.Temp), color = "darkred") + 
  labs(title="Mean Bearing Temp Leading up to a Bypass Error (extended)", x="Date", y="Temp (Celcius)")

ggplot(byp_subset1, aes(x=interval_time)) + 
  geom_point(aes(y = Oil.Temp), color = "darkblue") + 
  labs(title="Mean Oil Temp Leading up to a Bypass Error", x="Date", y="Temp (Celcius)")

ggplot(byp_subset2, aes(x=interval_time)) + 
  geom_point(aes(y = Oil.Temp), color = "darkblue") + 
  labs(title="Mean Oil Temp Leading up to a Bypass Error (extended)", x="Date", y="Temp (Celcius)")

ggplot(byp_subset1, aes(x=interval_time)) + 
  geom_point(aes(y = mean_wind), color = "darkgreen") + 
  labs(title="Mean Windspeed Leading up to a Bypass Error", x="Date", y="MPH")

ggplot(byp_subset1, aes(x=interval_time)) + 
  geom_point(aes(y = psi), color = "orange") + 
  labs(title="Mean Hydraulic Pressure Leading up to a Bypass Error", x="Date", y="PSI")

ggplot(byp_subset2, aes(x=interval_time)) + 
  geom_point(aes(y = psi), color = "orange") + 
  labs(title="Mean Hydraulic Pressure Leading up to a Bypass Error (extended)", x="Date", y="PSI")
#NOTE, the above plot has one additional error message occurrence on the 13th

ggplot(byp_subset1, aes(x=interval_time)) + 
  geom_point(aes(y = kilowatts), color = "purple") + 
  labs(title="Mean Active Power Leading up to a Bypass Error", x="Date", y="Kilowatts")

ggplot(byp_subset2, aes(x=interval_time)) + 
  geom_point(aes(y = kilowatts), color = "purple") + 
  labs(title="Mean Active Power Leading up to a Bypass Error (extended)", x="Date", y="Kilowatts")
#NOTE, the above plot has one additional error message occurrence on the 13th

# ggplot(byp_subset1, aes(x=interval_time)) + 
#   geom_point(aes(y = ambient_temp), color = "orange") + 
#   labs(title="Mean Ambient Temp Leading up to a Bypass Error", x="Date", y="Temp (Celcius)")
turbine7_data <- bypass_data[bypass_data$TurbineName == "Turbine 7",]

ggplot(turbine7_data, aes(x=interval_time)) + 
  geom_point(aes(y = Oil.Temp), color = "purple", size=0.25) + 
  labs(title="Temp Over Time", x="Date", y="Temperature")

month_interval1 <- interval(ymd("2021-01-01"), ymd("2021-02-01"))

month_subset1 <- turbine7_data[turbine7_data$interval_time %within% month_interval1,]

ggplot(month_subset1, aes(x=interval_time)) + 
  geom_point(aes(y = Oil.Temp), color = "darkred", size=0.5) + 
  labs(title="Temp over 1 month", x="Date", y="Temperature")
