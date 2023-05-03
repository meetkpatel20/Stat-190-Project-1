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
                             ntree = 400,
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

pi_hat <- predict(forest1, test_data, type = "prob")[,1]

rocCurve <- roc(response = test_data$ErrorType_Balance.of.Plant,
                predictor = pi_hat,
                levels = c("0", "1"))

plot(rocCurve, print.thres = T, print.auc = T)
