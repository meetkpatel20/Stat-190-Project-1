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
library(xts)
library(zoo)

full_data <- read.csv('ModellingData.csv', header=T)

# subsetting columns to only grab our plant fault code and expl vars
plant_data <- full_data[, c(1,2,3,4,5,6,7,8,12)]
plant_data <- plant_data[plant_data$TurbineName == "Turbine 27",]
turbine_data <- plant_data[,-c(1)]
# turbine_data$interval_time <- ymd_hms(turbine_data$interval_time)

library(zoo)
library(stats)
library(xts)

# Convert interval_time to POSIXct format
turbine_data$interval_time <- as.POSIXct(turbine_data$interval_time, format = "%Y-%m-%d %H:%M:%S")


# Set interval_time as the index
turbine_data_xts <- xts(turbine_data[-1], order.by = turbine_data$interval_time)

# Make index entries unique
idx <- index(turbine_data_xts)
idx[duplicated(idx)] <- idx[duplicated(idx)] + seq(0, by = 0.0000001, length.out = sum(duplicated(idx)))
index(turbine_data_xts) <- idx

# Impute missing values using the previous non-missing value
turbine_data_xts <- na.locf(turbine_data_xts)

# Lag the variables by 36 time points (6 hours)
turbine_data_lag <- stats::lag(turbine_data_xts, k = 36)

# Convert xts object to data frame
turbine_data_df <- as.data.frame(turbine_data_lag, row.names = TRUE)

# Convert row names to a column
turbine_data_df$datetime <- row.names(turbine_data_df)

# Convert datetime column to POSIXct format
turbine_data_df$datetime <- as.POSIXct(turbine_data_df$datetime, format = "%Y-%m-%d %H:%M:%S")

# Remove row names
row.names(turbine_data_df) <- NULL

# creating forest_data
forest_data <- turbine_data_df[,-c(8)]
forest_data$ErrorType_Balance.of.Plant <- as.factor(forest_data$ErrorType_Balance.of.Plant)
library(caret)
library(RANN)

# Identify variables with all missing values
missing_vars <- colnames(forest_data)[!apply(!is.na(forest_data), 2, any)]

# Remove variables with all missing values from the dataset
forest_data <- forest_data[, !(names(forest_data) %in% missing_vars)]

# Identify variables with near-zero variance or are all the same value
nzv <- nearZeroVar(forest_data, saveMetrics = TRUE)

# Get variable names that have zero variance or are all the same value
nzv_vars <- rownames(nzv[nzv$zeroVar | nzv$sameVar, ])

# Remove identified variables from the dataset
forest_data <- forest_data[, !(names(forest_data) %in% nzv_vars)]

# Impute missing values with mean of that variable
preProcValues <- preProcess(forest_data, method = c("center", "scale", "medianImpute"))
forest_data_imputed <- predict(preProcValues, forest_data)
forest_data_imputed <- na.omit(forest_data_imputed)

# Split data into training and testing sets
set.seed(123)
train_index <- createDataPartition(forest_data_imputed$ErrorType_Balance.of.Plant, p = 0.8, list = FALSE)
train_data <- forest_data_imputed[train_index, ]
test_data <- forest_data_imputed[-train_index, ]

## RANDOM FOREST 1
forest_model <- randomForest(ErrorType_Balance.of.Plant ~ .,
                             data = train_data,
                             ntree = 200,
                             mtry = 3,
                             importance = TRUE,
                             na.action = na.exclude)

# Print OOB error rate and confusion matrix
print(forest_model)
print(confusionMatrix(predict(forest_model), train_data$ErrorType_Balance.of.Plant))

## RANDOM FOREST 2 (WITH TUNING)

mtry <- c(1:6)

keeps <- data.frame(m=rep(NA, length(mtry)),
                    OOB_error_rate = rep(NA, length(mtry)))
for (idx in 1:length(mtry)){
  print(paste0("Fitting m = ", mtry[idx]))
  
  tempforest <- randomForest(ErrorType_Balance.of.Plant ~ . ,
                             data = train_data,
                             ntree = 200,
                             mtry = mtry[idx])
  
  keeps[idx, "m"] <- mtry[idx]
  keeps[idx, "OOB_error_rate"] <- mean(predict(tempforest) != train_data$ErrorType_Balance.of.Plant)
  
}
keeps
plot(keeps, type="l", main="OOB Error Rate w/ Different #'s of Vars")

pi_hat <- predict(forest_model, test_data, type = "prob")[,"1"]

rocCurve <- roc(response = test_data$ErrorType_Balance.of.Plant,
                predictor = pi_hat,
                levels = c("0", "1"))

plot(rocCurve, print.thres = T, print.auc = T)


# -------------------------------------------------------------------------------


## WITH MORE X VARS
weather_data <- read.csv("ModelData_weather.csv")
weather_data <- weather_data[,-c(9, 10, 11, 12)]

# Checking the error occurences per Turbine
counting_errors <- weather_data %>%
  filter(ErrorOccurrence == "yes") %>%
  group_by(TurbineName) %>%
  summarise(error_count = n())

counting_errors

counting_nos <- weather_data %>%
  filter(ErrorOccurrence == "no") %>%
  group_by(TurbineName) %>%
  summarise(error_count = n())

counting_nos

# Just looking at Turbine 27
weather_27 <- weather_data[weather_data$TurbineName == "Turbine 27",]
weather_27 <- weather_27[,-c(1)] # take out TurbineName
# Convert interval_time to POSIXct format
weather_27$interval_time <- ymd_hms(weather_27$interval_time)

# Set interval_time as the index
weather_27_xts <- xts(weather_27[-1], order.by = weather_27$interval_time)

# Make index entries unique
idx <- index(weather_27_xts)
idx[duplicated(idx)] <- idx[duplicated(idx)] + seq(0, by = 0.0000001, length.out = sum(duplicated(idx)))
index(weather_27_xts) <- idx

# Impute missing values using the previous non-missing value
weather_27_xts <- na.locf(weather_27_xts)

# Lag the variables by 36 time points (6 hours)
weather_27_lag <- stats::lag(weather_27_xts, k = 36)

# Convert xts object to data frame
weather_27_df <- data.frame(weather_27_lag)

# Create new column with interval_time in POSIXct format
weather_27_df$interval_time <- as.POSIXct(gsub("X", "", row.names(weather_27_df)), format = "%Y.%m.%d.%H.%M.%S")

# Remove row names
row.names(weather_27_df) <- NULL

# Remove first 36 rows with NAs
weather_27_df <- weather_27_df[37:nrow(weather_27_df),]

# Remove interval_time column
weather_27_df <- weather_27_df[, -c(51)]

# Converting our vars to the correct datatypes
weather_27_df$ErrorOccurrence <- as.factor(weather_27_df$ErrorOccurrence)

weather_27_df <- weather_27_df %>% 
  mutate_at(vars(1:6), as.numeric) %>%
  mutate_at(vars(8:45), as.numeric) %>%
  mutate_at(vars(46:47), ~if_else(. == "True", TRUE, FALSE)) %>%
  mutate_at(vars(49:50), as.numeric)

# Creating forest_data_27 for the Turbine 27 data
forest_data_27 <- weather_27_df

# Taking out the char timestamp column for the forests
forest_data_27 <- forest_data_27[,-c(48)] # column: max_gust_localts

## TURBINE 27 FORESTS
# Split data into training and testing sets
set.seed(123)
train_index <- createDataPartition(forest_data_27$ErrorOccurrence, p = 0.8, list = FALSE)
train_data <- forest_data_27[train_index, ]
test_data <- forest_data_27[-train_index, ]

## RF1 (Turbine 27) NO TUNING
rf1 <- randomForest(ErrorOccurrence ~ .,
                             data = train_data,
                             ntree = 1000,
                             mtry = sqrt(48),
                             importance = TRUE,
                             na.action = na.exclude)

# Print OOB error rate and confusion matrix
print(rf1)

## TUNING Turbine 27 RF:
mtry <- c(1:48)

keeps <- data.frame(m=rep(NA, length(mtry)),
                    OOB_error_rate = rep(NA, length(mtry)))
for (idx in 1:length(mtry)){
  print(paste0("Fitting m = ", mtry[idx]))
  
  tempforest <- randomForest(ErrorOccurrence ~ . ,
                             data = train_data,
                             ntree = 1000,
                             mtry = mtry[idx])
  
  keeps[idx, "m"] <- mtry[idx]
  keeps[idx, "OOB_error_rate"] <- mean(predict(tempforest) != train_data$ErrorOccurrence)
  
}
keeps
plot(keeps)
# BEST mtry = 20 (lowest OOB error = 0.01012319)

## RF 2 (WITH TUNING):
rf2 <- randomForest(ErrorOccurrence ~ .,
                    data = train_data,
                    ntree = 1000,
                    mtry = 20, # TUNED!
                    importance = TRUE,
                    na.action = na.exclude)

# Print OOB error rate and confusion matrix
print(rf2)

# Tuning pi_star by getting the ROC curve info
pi_hat <- predict(rf2, test_data, type = "prob")[,1]

rocCurve <- roc(response = test_data$ErrorOccurrence,
                predictor = pi_hat,
                levels = c("no", "yes"))

plot(rocCurve, print.thres = T, print.auc = T)

# Extract pi_star in an automated way
pi_star <- coords(rocCurve, "best", ret = "threshold")$threshold[1]
test_data$forest_pred <- as.factor(ifelse(pi_hat < pi_star, "yes", "no"))

# Make predictions using the random forest model and forest_pred as a predictor
predictions <- predict(rf2, newdata = test_data, type = "response", predictors = c("forest_pred"))

# Create a confusion matrix
conf_mat <- table(test_data$ErrorOccurrence, test_data$forest_pred)
print(conf_mat)

# Calculate the accuracy of the model
accuracy <- sum(diag(conf_mat)) / sum(conf_mat)
cat("\nAccuracy: ", round(accuracy, 2))

varImpPlot(rf2, type=1, main="Turbine 27 Random Forest Variable Importance")


## RF #3 -- FURTHER TUNING WITH WIEGHTS
probs <- c("no" = 0.873, "yes" = 0.127)
rf3 <- randomForest(ErrorOccurrence ~ .,
                    data = train_data,
                    ntree = 1000,
                    mtry = 20,
                    classwt = probs,
                    importance = TRUE,
                    na.action = na.exclude)

# Print OOB error rate and confusion matrix
print(rf3)


# -----------------------------------------------------------------------------


### MODELS USING TURBINE 31:
all_data <- weather_data
all_data <- all_data[all_data$TurbineName == "Turbine 31",]
all_data <- all_data[,-c(1)] # take out TurbineName

# Convert interval_time to POSIXct format
all_data$interval_time <- ymd_hms(all_data$interval_time)

# Set interval_time as the index
weather_all_xts <- xts(all_data[-1], order.by = all_data$interval_time)

# Make index entries unique
idx <- index(weather_all_xts)
idx[duplicated(idx)] <- idx[duplicated(idx)] + seq(0, by = 0.0000001, length.out = sum(duplicated(idx)))
index(weather_all_xts) <- idx

# Impute missing values using the previous non-missing value
weather_all_xts <- na.locf(weather_all_xts)

# Lag the variables by 36 time points (6 hours)
weather_all_lag <- stats::lag(weather_all_xts, k = 36)

# Convert xts object to data frame
weather_all_df <- data.frame(weather_all_lag)

# Create new column with interval_time in POSIXct format
weather_all_df$interval_time <- as.POSIXct(gsub("X", "", row.names(weather_all_df)), format = "%Y.%m.%d.%H.%M.%S")

# Remove row names
row.names(weather_all_df) <- NULL

# Remove first 36 rows with NAs
weather_all_df <- weather_all_df[37:nrow(weather_all_df),]

# Remove interval_time column
weather_all_df <- weather_all_df[, -c(51)]

# Converting our vars to the correct datatypes
weather_all_df$ErrorOccurrence <- as.factor(weather_all_df$ErrorOccurrence)

weather_all_df <- weather_all_df %>% 
  mutate_at(vars(1:6), as.numeric) %>%
  mutate_at(vars(8:45), as.numeric) %>%
  mutate_at(vars(46:47), ~if_else(. == "True", TRUE, FALSE)) %>%
  mutate_at(vars(49:50), as.numeric)

# Creating forest_data_27 for the Turbine 27 data
forest_data_all <- weather_all_df

# Taking out the char timestamp column for the forests
forest_data_all <- forest_data_all[,-c(48)] # column: max_gust_localts

## ALL TURBINES Random Forest Models
set.seed(123)
train_index <- createDataPartition(forest_data_all$ErrorOccurrence, p = 0.8, list = FALSE)
train_data <- forest_data_all[train_index, ]
test_data <- forest_data_all[-train_index, ]

## RF1 (Turbine 31) NO TUNING
rf1_all <- randomForest(ErrorOccurrence ~ .,
                    data = train_data,
                    ntree = 1000,
                    mtry = sqrt(48),
                    importance = TRUE,
                    na.action = na.exclude)

# Print OOB error rate and confusion matrix
print(rf1_all)

## TUNING ALL DATA RF:
mtry <- c(1:48)

keeps <- data.frame(m=rep(NA, length(mtry)),
                    OOB_error_rate = rep(NA, length(mtry)))
for (idx in 1:length(mtry)){
  print(paste0("Fitting m = ", mtry[idx]))
  
  tempforest <- randomForest(ErrorOccurrence ~ . ,
                             data = train_data,
                             ntree = 1000,
                             mtry = mtry[idx])
  
  keeps[idx, "m"] <- mtry[idx]
  keeps[idx, "OOB_error_rate"] <- mean(predict(tempforest) != train_data$ErrorOccurrence)
  
}
keeps
plot(keeps, type="l")
# BEST mtry = 3


## RF2 (Turbine 31) TUNED!
rf2_all <- randomForest(ErrorOccurrence ~ .,
                        data = train_data,
                        ntree = 1000,
                        mtry = 3, # from tuning OOB error
                        importance = TRUE,
                        na.action = na.exclude)

# Print OOB error rate and confusion matrix
print(rf2_all)

# Tuning pi_star by getting the ROC curve info
pi_hat <- predict(rf2_all, test_data, type = "prob")[,1]

rocCurve <- roc(response = test_data$ErrorOccurrence,
                predictor = pi_hat,
                levels = c("no", "yes"))

plot(rocCurve, print.thres = T, print.auc = T)

# Extract pi_star in an automated way
pi_star <- coords(rocCurve, "best", ret = "threshold")$threshold[1]
test_data$forest_pred <- as.factor(ifelse(pi_hat < pi_star, "yes", "no"))

# Make predictions using the random forest model and forest_pred as a predictor
predictions <- predict(rf2, newdata = test_data, type = "response", predictors = c("forest_pred"))

# Create a confusion matrix
conf_mat <- table(test_data$ErrorOccurrence, test_data$forest_pred)
print(conf_mat)

# Calculate the accuracy of the model
accuracy <- sum(diag(conf_mat)) / sum(conf_mat)
cat("\nAccuracy: ", round(accuracy, 2))

varImpPlot(rf2, type=1, main="Turbine 31 Random Forest Variable Importance")



# -----------------------------------------------------------------------------


### MODELS USING ALL TURBINES:
all_data <- weather_data
all_data <- all_data[,-c(1)] # take out TurbineName

# Convert interval_time to POSIXct format
all_data$interval_time <- ymd_hms(all_data$interval_time)

# Set interval_time as the index
weather_all_xts <- xts(all_data[-1], order.by = all_data$interval_time)

# Make index entries unique
idx <- index(weather_all_xts)
idx[duplicated(idx)] <- idx[duplicated(idx)] + seq(0, by = 0.0000001, length.out = sum(duplicated(idx)))
index(weather_all_xts) <- idx

# Impute missing values using the previous non-missing value
weather_all_xts <- na.locf(weather_all_xts)

# Lag the variables by 36 time points (6 hours)
weather_all_lag <- stats::lag(weather_all_xts, k = 36)

# Convert xts object to data frame
weather_all_df <- data.frame(weather_all_lag)

# Create new column with interval_time in POSIXct format
weather_all_df$interval_time <- as.POSIXct(gsub("X", "", row.names(weather_all_df)), format = "%Y.%m.%d.%H.%M.%S")

# Remove row names
row.names(weather_all_df) <- NULL

# Remove first 36 rows with NAs
weather_all_df <- weather_all_df[37:nrow(weather_all_df),]

# Remove interval_time column
weather_all_df <- weather_all_df[, -c(51)]

# Converting our vars to the correct datatypes
weather_all_df$ErrorOccurrence <- as.factor(weather_all_df$ErrorOccurrence)

weather_all_df <- weather_all_df %>% 
  mutate_at(vars(1:6), as.numeric) %>%
  mutate_at(vars(8:45), as.numeric) %>%
  mutate_at(vars(46:47), ~if_else(. == "True", TRUE, FALSE)) %>%
  mutate_at(vars(49:50), as.numeric)

# Creating forest_data_27 for the Turbine 27 data
forest_data_all <- weather_all_df

# Taking out the char timestamp column for the forests
forest_data_all <- forest_data_all[,-c(48)] # column: max_gust_localts

## ALL TURBINES Random Forest Models
set.seed(123)
train_index <- createDataPartition(forest_data_all$ErrorOccurrence, p = 0.8, list = FALSE)
train_data <- forest_data_all[train_index, ]
test_data <- forest_data_all[-train_index, ]

## RF1 (ALL Turbines) NO TUNING
rf1_all <- randomForest(ErrorOccurrence ~ .,
                        data = train_data,
                        ntree = 200,
                        mtry = sqrt(48),
                        importance = TRUE,
                        na.action = na.exclude)

# Print OOB error rate and confusion matrix
print(rf1_all)

## TUNING ALL DATA RF:
mtry <- c(1:48)

keeps <- data.frame(m=rep(NA, length(mtry)),
                    OOB_error_rate = rep(NA, length(mtry)))
for (idx in 1:length(mtry)){
  print(paste0("Fitting m = ", mtry[idx]))
  
  tempforest <- randomForest(ErrorOccurrence ~ . ,
                             data = train_data,
                             ntree = 200,
                             mtry = mtry[idx])
  
  keeps[idx, "m"] <- mtry[idx]
  keeps[idx, "OOB_error_rate"] <- mean(predict(tempforest) != train_data$ErrorOccurrence)
  
}
keeps
plot(keeps, type="l")
# BEST mtry = 3
smallest <- min(keeps$OOB_error_rate)
best <- which(keeps$OOB_error_rate==smallest)

## RF2 (ALL Turbines) TUNED!
rf2_all <- randomForest(ErrorOccurrence ~ .,
                        data = train_data,
                        ntree = 200,
                        mtry = best, # from tuning OOB error
                        importance = TRUE,
                        na.action = na.exclude)

# Print OOB error rate and confusion matrix
print(rf2_all)

# Tuning pi_star by getting the ROC curve info
pi_hat <- predict(rf2_all, test_data, type = "prob")[,1]

rocCurve <- roc(response = test_data$ErrorOccurrence,
                predictor = pi_hat,
                levels = c("no", "yes"))

plot(rocCurve, print.thres = T, print.auc = T)

# Extract pi_star in an automated way
pi_star <- coords(rocCurve, "best", ret = "threshold")$threshold[1]
test_data$forest_pred <- as.factor(ifelse(pi_hat < pi_star, "yes", "no"))

# Make predictions using the random forest model and forest_pred as a predictor
predictions <- predict(rf2, newdata = test_data, type = "response", predictors = c("forest_pred"))

# Create a confusion matrix
conf_mat <- table(test_data$ErrorOccurrence, test_data$forest_pred)
print(conf_mat)

# Calculate the accuracy of the model
accuracy <- sum(diag(conf_mat)) / sum(conf_mat)
cat("\nAccuracy: ", round(accuracy, 3))

varImpPlot(rf2, type=1, main="ALL Turbines Random Forest Variable Importance")


