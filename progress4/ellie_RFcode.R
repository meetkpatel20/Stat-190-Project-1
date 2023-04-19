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

full_data <- read.csv('ModellingFullData.csv', header=T)

# subsetting columns to only grab our plant fault code and expl vars
plant_data <- full_data[, c(1,2,3,4,5,6,7,8,9,10)]

plant_data$Date <- as.Date(as.POSIXct(plant_data$interval_time))
plant_data$interval_time <- ymd_hms(plant_data$interval_time)

# grab only the error data
error_data <- plant_data[plant_data$ErrorType_Balance.of.Plant == 1,]
# extracting first occurrence of a fault "event"
fault_events <- error_data %>%
  group_by(TurbineName, Date, ErrorType_Balance.of.Plant) %>%
  # taking the first occurrence of the day
  arrange(interval_time) %>%
  filter(row_number()==1)

# putting the data back together
no_errors <- plant_data[plant_data$ErrorType_Balance.of.Plant == 0,]
model_data <- rbind(no_errors, fault_events)


### MAKING RANDOM FOREST
forest_ready <- model_data[, c(3,4,5,6,7,8,10)]


RNGkind(sample.kind = "default")
set.seed(2291352)
train.idx <- sample(x = 1:nrow(forest_ready), size=0.8*nrow(forest_ready))
train.df <- forest_ready[train.idx,]
test.df <- forest_ready[-train.idx,]

forest1 <- randomForest(ErrorType_Balance.of.Plant ~ . ,
                       data = train.df,
                       ntree = 100,
                       mtry = sqrt(6),
                       importance = TRUE)

forest1
plot(forest1, main = "Error Code: 'Balance of Plant' RF Error")

# FOREST NUMBER 2
sub_plant_data <- forest_ready %>%
  mutate(error = if_else(ErrorType_Balance.of.Plant > 0, "Y", "N"))

sub_plant_data$error <- as.factor(sub_plant_data$error)

sub_plant_data <- sub_plant_data[,-c(7)] #removing the "old" Error col

RNGkind(sample.kind = "default")
set.seed(2291352)
train.idx <- sample(x = 1:nrow(sub_plant_data), size=0.8*nrow(sub_plant_data))
train.df <- sub_plant_data[train.idx,]
test.df <- sub_plant_data[-train.idx,]

forest2 <- randomForest(error ~ . ,
                              data = train.df,
                              ntree = 200,
                              mtry = 3,
                              importance = TRUE)

forest2

plot(forest2, main = "Error Code: 'Balance of Plant' RF Error (binary)")

mtry <- c(1:6)

keeps <- data.frame(m=rep(NA, length(mtry)),
                    OOB_error_rate = rep(NA, length(mtry)))
for (idx in 1:length(mtry)){
  print(paste0("Fitting m = ", mtry[idx]))
  
  tempforest <- randomForest(error ~ . ,
                             data = train.df,
                             ntree = 200,
                             mtry = mtry[idx])
  
  keeps[idx, "m"] <- mtry[idx]
  keeps[idx, "OOB_error_rate"] <- mean(predict(tempforest) != train.df$error)
  
}
keeps

# plot m vs. oob error rate
ggplot(data=keeps) +
  geom_line(aes(x=m, y=OOB_error_rate)) +
  scale_x_continuous(breaks=c(1:6)) +
  labs(x="m (mtry): # of x variables sampled",
       y="OOB Error Rate")

final_forest <- randomForest(error ~ .,
                             data=train.df,
                             ntree=200,
                             mtry=1, #from tuning mtry above
                             importance=TRUE)


plot(final_forest)
pi_hat <- predict(final_forest, test.df, type = "prob")[,2]
#pi_hat

rocCurve <- roc(response = test.df$error,
                predictor = pi_hat,
                levels = c("N", "Y"))

plot(rocCurve, print.thres = T, print.auc = T)
