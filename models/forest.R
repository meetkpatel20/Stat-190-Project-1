rm(list=ls())
# install.packages("randomForest")
# install.packages("tidytext")
# install.packages("pROC")
# install.packages("topicmodels")
# install.packages("reshape2")

library(randomForest)
library(ggplot2)
library(pROC)
library(dplyr)
library(tidytext)
library(topicmodels)
library(reshape2)

full_data <- read.csv("FullData2.csv", stringsAsFactors = TRUE)

#subsetting for "Plant Balance" Fault Code (column 231)
plant_data2 <- full_data[, c(2,3,4,5,6,7,8,9,10,11,231)]
plant_data <- plant_data2[,-c(1,2,3)] # quantitative variables only
plant_data <- plant_data2[,-c(1,2,3, 7, 8, 9)] # taking out ambient_temp, active
# power, and hydraulic pressure

#exploring the data a little
# print(plant_data[plant_data$ErrorType_Balance.of.Plant > 0, c('date')])
# unique(gear_oiltemp_data$ErrorMessage_Gear.Oil.Temperature.High)

# making our y variable binary
# gear_oiltemp_data <- gear_oiltemp_data %>% 
#   mutate(error = if_else(ErrorMessage_Gear.Oil.Temperature.High > 0, 1, 0))
# 
# gear_oiltemp_data <- gear_oiltemp_data[,-c(5)]
# unique(gear_oiltemp_data$error)


sub_plant_data <- plant_data %>%
  mutate(error = if_else(ErrorType_Balance.of.Plant > 0, "1", "0"))

sub_plant_data <- sub_plant_data[,-c(5)]
plant_data <- plant_data[,-c(4)]


RNGkind(sample.kind = "default")
set.seed(2291352)
train.idx <- sample(x = 1:nrow(plant_data), size=0.8*nrow(plant_data))
train.df <- plant_data[train.idx,]
test.df <- plant_data[-train.idx,]

forest <- randomForest(ErrorType_Balance.of.Plant ~ . ,
                             data = train.df,
                             ntree = 100,
                             mtry = sqrt(6),
                             importance = TRUE,
                             na.action = na.exclude)

forest

plot(forest, main = "Balancing Plant Error RF (without Ambient Temp)")

# trying a tree again with binary error variable:
RNGkind(sample.kind = "default")
set.seed(2291352)
train.idx <- sample(x = 1:nrow(sub_plant_data), size=0.8*nrow(sub_plant_data))
train.df <- sub_plant_data[train.idx,]
test.df <- sub_plant_data[-train.idx,]

first_forest2 <- randomForest(error ~ . ,
                             data = train.df,
                             ntree = 100,
                             mtry = 2,
                             importance = TRUE,
                             na.action = na.exclude)

first_forest2

plot(first_forest2, main = "Error Code: 'Balance of Plant' RF Error")



mtry <- c(1:4)

keeps <- data.frame(m=rep(NA, length(mtry)),
                    OOB_error_rate = rep(NA, length(mtry)))
for (idx in 1:length(mtry)){
  print(paste0("Fitting m = ", mtry[idx]))
  
  tempforest <- randomForest(error ~ . ,
                             data = train.df,
                             ntree = mtry[idx])
  
  keeps[idx, "m"] <- mtry[idx]
  keeps[idx, "OOB_error_rate"] <- mean(predict(tempforest) != train.df$error)
  
}
keeps

pi_hat <- predict(forest, test.df, type = "vote")[,1]

rocCurve <- roc(response = test.df$error,
                predictor = pi_hat,
                levels = c("0", "1"))

plot(rocCurve, print.thres = T, print.auc = T)
