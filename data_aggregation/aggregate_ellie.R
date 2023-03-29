library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
# power_data$Time = str_sub(power_data$Time,1,-7)
# pressure_data$Time = str_sub(pressure_data$Time,1,-7)

# active_pow1 <- read.csv('active_power1.csv', header=T)
# active_pow2 <- read.csv('active_power2.csv', header=T)
# active_pow3 <- read.csv('active_power3.csv', header=T)

hyd_pressure1 <- read.csv('hyd_pressure1.csv', header=T)
hyd_pressure2 <- read.csv('hyd_pressure2.csv', header=T)
hyd_pressure3 <- read.csv('hyd_pressure3.csv', header=T)

all_pressure <- rbind(hyd_pressure1, hyd_pressure2, hyd_pressure3)
all_power <- read.csv('all_active_power.csv', header = T)
colnames(all_power)[5] <- "kilowatts"


write.csv(all_pressure, 'all_hyd_pressure.csv')

all_power$Time = ymd_hms(all_power$Time)
all_power$interval_time = round_date(all_power$Time, "10 mins")
all_pressure$Time = ymd_hms(all_pressure$Time)
all_pressure$interval_time = round_date(all_pressure$Time, "10 mins")

write.csv(all_pressure, 'all_hyd_pressure.csv')
write.csv(all_power, 'all_power.csv')

ambient_raw <- read.csv('AmbientTemp.csv', header=T)
ambient_raw$Timestamp = ymd_hms(ambient_raw$Timestamp)
ambient_raw$interval_time = round_date(ambient_raw$Timestamp, "10 mins")
ambient_subset <- subset(ambient_raw, select = -c(Timestamp, Date, Ambient.temp))
colnames(ambient_subset)[2] <- "ambient_temp"

other_turbines <- read.csv('meet_gonzalo.csv', header = T)

all_pressure <- read.csv('all_hyd_pressure.csv', header = TRUE)
all_power <- read.csv('all_active_power.csv', header = TRUE)
# merged_data = full_join(pressure_data, power_data, by = c("Turbine", "Date", "Time"))
simplified_pressure = subset(all_pressure, select = -c(X, Date, Time, Error.Type) )
simplified_power = subset(all_power, select = -c(X, Date, Time, Error.Type) )

power_agg = aggregate(simplified_power,
                by = list(simplified_power$Turbine, simplified_power$interval_time),
                FUN = mean)

pressure_agg = aggregate(simplified_pressure,
                      by = list(simplified_pressure$Turbine, simplified_pressure$interval_time),
                      FUN = mean)

ambient_agg = aggregate(ambient_subset,
                        by = list(ambient_subset$Turbine, ambient_subset$interval_time),
                        FUN = mean)

simp_power = subset(power_agg, select = -c(Turbine, interval_time) )
simp_pressure = subset(pressure_agg, select = -c(Turbine, interval_time) )
simp_ambient = subset(ambient_agg, select = -c(Turbine, interval_time))

colnames(simp_power)[3] <- "kilowatts"
colnames(simp_power)[2] <- "interval_time"
colnames(simp_power)[1] <- "Turbine"
colnames(simp_pressure)[3] <- "psi"
colnames(simp_pressure)[2] <- "interval_time"
colnames(simp_pressure)[1] <- "Turbine"
colnames(simp_ambient)[2] <- "interval_time"
colnames(simp_ambient)[1] <- "Turbine"

# write.csv(simp_power, 'master_power.csv')
# write.csv(simp_pressure, 'master_pressure.csv')

full_data <- read.csv('FullData.csv', header=T)
full_subset <- subset(full_data, select = -c(X, rpm, psi) )
power <- read.csv("master_power.csv", header=T)
pressure <- read.csv("master_pressure.csv", header=T)
power <- subset(power, select = -c(X) )
pressure <- subset(pressure, select = -c(X) )

other_turbines$interval_time <- as.POSIXct( other_turbines$interval_time, tz = "UTC" )
first_merge <- left_join(full_subset, power, by = join_by(TurbineName==Turbine, interval_time==interval_time))
# simplified_pressure$interval_time <- as.POSIXct( simplified_pressure$interval_time, tz = "UTC" )
FULL_DATASET <- left_join(first_merge, pressure, by = join_by(TurbineName==Turbine, interval_time==interval_time))

write.csv(FULL_DATASET, 'master_with_psi_power.csv')

FULL_DATASET$interval_time <- as.POSIXct( FULL_DATASET$interval_time, tz = "UTC" )
with_ambient <- left_join(FULL_DATASET, simp_ambient, by = join_by(TurbineName==Turbine, interval_time==interval_time))

reordering1 <- with_ambient %>% relocate(ambient_temp, .before = Oil.Temp)
reordering2 <- reordering1 %>% relocate(psi, .before = Oil.Temp)
reordering3 <- reordering2 %>% relocate(kilowatts, .before = Oil.Temp)

write.csv(reordering3, 'FullData2.csv')

plot(rpm, mean_wind, main="Power vs Wind",
     xlab="Mean RPM ", ylab="Mean Windspeed ")

library(ggplot2)
ggplot(full_data, aes(x = rpm, y = mean_wind)) + 
  geom_point(size = 0.5, color = "#0099f9") +
  labs(title= "Mean Power (rpm) vs. Mean Windspeed (mph)")

ggplot(full_data, aes(x = rpm, y = psi)) + 
  geom_point(size = 0.5, color = "dark red") +
  labs(title= "Mean Power (rpm) vs. Mean Hydraulic Pressure (psi)")

ggplot(full_data, aes(x = rpm, y = Oil.Temp)) + 
  geom_point(size = 0.5, color = "dark green") +
  labs(title= "Mean Power (rpm) vs. Mean Oil Temperature (degrees C)")

ggplot(full_data, aes(x = rpm, y = IMS.Bearing.Temp)) + 
  geom_point(size = 0.5, color = "dark blue") +
  labs(title= "Mean Power (rpm) vs. Mean IMS Bearing Temp (degrees C)")

meet_gonzalo <- read.csv('meet_gonzalo.csv', header=T)

meet_gonzalo1 = left_join(meet_gonzalo, power_data, by = join_by(TurbineName==Turbine, interval_time==Time))

meet_gonzalo2 = left_join(meet_gonzalo1, pressure_data, by = join_by(TurbineName==Turbine, interval_time==Time))

full_data = subset(meet_gonzalo2, select = -c(Date.x,Date.y,Error.Type.x,Error.Type.y))

full_data$rpm[is.na(full_data$rpm)]<-mean(full_data$rpm,na.rm=TRUE)
full_data$psi[is.na(full_data$psi)]<-mean(full_data$psi,na.rm=TRUE)
full_data$mean_RepPerMinute[is.na(full_data$mean_RepPerMinute)]<-mean(full_data$mean_RepPerMinute,na.rm=TRUE)




write.csv(full_data, "master_with_nulls.csv")

master_data <- read.csv('FullAllFaultCodesTimeAggregated.csv')
colnames(full_data)[7] <- "kilowatts" #fixing column name

write.csv(master_data, 'master_fixed_units.csv')
