# Predicting Balance of Plant Wind Turbine Errors 

This repository contains code and documentation for a project that aims to predict Balance of Plant (BoP) errors in wind turbines using datasets acquired from Berkshire Hathaway Energy (BHE).

## Introduction
### Business Problem
Balance of Plant errors encompass important diagnostic information about the performance and state of auxiliary systems in regard to operating turbines. Predicting BoP errors can help save repair costs, time, improve safety, and increase the rate of investment. By catching and repairing an error before any actual problems or danger occurs, technicians can prepare the necessary equipment prior to any issue, saving time and money to fix the problem and mitigate any potential financial loss.

## Data
The project used turbine sensor reading data and fault error data provided by Berkshire Hathaway Energy. Both datasets were ordered by Turbine Name and a date/timestamp, so they were combined into one dataset using those columns as keys. The explanatory, quantitative variables in the sensor data we used are Gearbox HS Bearing Temperature, Gearbox IMS Bearing Temperature, Ambient Temperature, Gearbox Oil Temperature, Active Power, Hydraulic Pressure, and Generator RPM. In the fault error dataset, it contained the fault code/type, which we transformed into binary dummy variables for each unique fault code. We also included summary statistics of the sensor data by extracting the minimum, maximum and the mean of all the six sensor data variables.

## Data Exploration & Findings
### Cleaning
The sensor and fault error data were initially given in the format of folders with several csv files for each quantitative variable. To make this useful, we had to merge the set of files for each variable into one dataset, then merge the variable datasets together by timestamp into one bigger data frame. We also decided to aggregate the data by 10-minute intervals. However, our decision to do small, 10-minute intervals proved to be more challenging because of how large our datasets are.

### Exploration & Findings
After cleaning and merging the data, we explored the relationships between our explanatory variables and the target variable, Balance of Plant errors. We found that the Gearbox Oil Temperature, Ambient Temperature, and Hydraulic Pressure were...

## Libraries Used
The following R packages were used in the project:

ggplot2
lubridate
tidyverse
gridExtra
dplyr
hrbrthemes
magrittr
randomForest
pROC
MASS
rlist
e1071
caTools
class
## Files

bhe_data.csv: The final dataset used for the models, containing over 56 explanatory variables.
code/: The directory containing all the code files for cleaning, preprocessing and modeling.
documentation/: The directory containing all the documentation files for the project.
README.md: This file.

## Acknowledgements
We would like to acknowledge Berkshire Hathaway Energy for providing the data and our team members for their contributions to this project.
