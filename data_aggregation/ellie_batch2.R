rm(list=ls())
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)

# load it all in
hyd_pressure <- read.csv('batch2_pressure.csv', header=T)
active_power <- read.csv('batch2_power.csv', header=T)
ambient_temp <- read.csv('batch2_ambient.csv', header=T)

# converting time to timestamp and creating interval_time (10 minute intervals)
hyd_pressure$Time = ymd_hms(hyd_pressure$Time)
hyd_pressure$interval_time = round_date(hyd_pressure$Time, "10 mins")
active_power$Time = ymd_hms(active_power$Time)
active_power$interval_time = round_date(active_power$Time, "10 mins")
ambient_temp$Time = ymd_hms(ambient_temp$Time)
ambient_temp$interval_time = round_date(ambient_temp$Time, "10 mins")


# subsetting to just the columns we need for aggregating
simp_pressure = subset(hyd_pressure, select = -c(Date, Time, Error.Type) )
simp_power = subset(active_power, select = -c(Date, Time, Error.Type) )
simp_ambient = subset(ambient_temp, select = -c(Date, Time, Error.Type) )


power_agg = aggregate(simp_power,
                      by = list(simp_power$Turbine, simp_power$interval_time),
                      FUN = mean)

pressure_agg = aggregate(simp_pressure,
                         by = list(simp_pressure$Turbine, simp_pressure$interval_time),
                         FUN = mean)

ambient_agg = aggregate(simp_ambient,
                        by = list(simp_ambient$Turbine, simp_ambient$interval_time),
                        FUN = mean)

# cleaning up the columns post-aggregating
power_agg = subset(power_agg, select = -c(Turbine, interval_time) )
pressure_agg = subset(pressure_agg, select = -c(Turbine, interval_time) )
ambient_agg = subset(ambient_agg, select = -c(Turbine, interval_time))

colnames(power_agg)[2] <- "interval_time"
colnames(power_agg)[1] <- "Turbine"
colnames(pressure_agg)[2] <- "interval_time"
colnames(pressure_agg)[1] <- "Turbine"
colnames(ambient_agg)[2] <- "interval_time"
colnames(ambient_agg)[1] <- "Turbine"

# joining all 3 datasets together by time interval/time stamp
first_merge <- full_join(power_agg, pressure_agg, by = join_by(Turbine==Turbine, interval_time==interval_time))
power_pressure_ambient <- full_join(first_merge, ambient_agg, by = join_by(Turbine==Turbine, interval_time==interval_time))

write.csv(power_pressure_ambient, "power_pressure_ambient.csv")


# merging with Meet's aggregated IMS and Oil Temp Data
ims_oil_agg <- read.csv('ims_oil_agg.csv', header=T)
ims_oil_agg$TimeStamp <- as.POSIXct(ims_oil_agg$TimeStamp, tz = "UTC" )

ellie_meet <- full_join(power_pressure_ambient, ims_oil_agg, by = join_by(Turbine==TurbineName, interval_time==TimeStamp))

# cleaning up the columns one last time
ellie_meet <- subset(ellie_meet, select = -c(X) )

write.csv(ellie_meet, "meet_ellie_aggregated.csv")
