install.packages('aod')
install.packages('ggplot2')
install.packages('ISLR')
install.packages('dplyr')
install.packages('car')
library(dplyr)
install.packages("ggplot2")
install.packages("corrplot")
library("corrplot")

inputData <- read.csv("G:/David/JCU/CITY OF SACREMENTO DATA/CSV/chunk4-vehicles.csv", header = T, sep = ",")
head(inputData)
names(inputData)
summary(inputData)
#check class bias
#table(inputData$is_double_parked)
#False    True 
#3625371  791923 
#Clearly, there is a class bias, So must sample the observations in approximately equal proportions to get better models.

# Create Training Data
library(forcats)
#inputData <- recode_factor(inputData$is_parked, "False" = 0, "True" = 1)
#install.packages('dplyr')
inputData$is_parked <- as.logical(inputData$is_parked)
inputData$is_parked <- as.numeric(inputData$is_parked)
inputData$is_double_parked <- as.logical(inputData$is_double_parked)
inputData$is_double_parked <- as.numeric(inputData$is_double_parked)
#inputData[,'is_double_parked'] <- lapply(inputData[,'is_double_parked'], as.numeric)
head(inputData)

sapply(inputData, class)
inputData$trackedID <- as.numeric(inputData$trackedID)
inputData$frame_num <- as.numeric(inputData$frame_num)
inputData$bbox_xmin <- as.numeric(inputData$bbox_xmin)
inputData$bbox_xmax <- as.numeric(inputData$bbox_xmax)
inputData$bbox_ymin <- as.numeric(inputData$bbox_ymin)
inputData$bbox_ymax <- as.numeric(inputData$bbox_ymax)
inputData$num_objects_this_frame <- as.numeric(inputData$num_objects_this_frame)
inputData$total_objects_detected <- as.numeric(inputData$total_objects_detected)

set.seed(100) # set seed to replicate results
trainingIndex <- sample(1:nrow(inputData), 0.7*nrow(inputData)) # indices for 70% training data
trainingData <- inputData[trainingIndex, ] # training data
testData <- inputData[-trainingIndex, ] # test data

mydata <- data.matrix(inputData)

corr <- round(cor(mydata),2)
head(corr[,1:15])

parked <- mydata[, c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)]
corr <- round(cor(parked), 2)
head(corr[,1:15])
corrplot(corr, method = "circle")

parked1 <- mydata[, c(3,4,5,11,12,13,14,15)]
corr1 <- round(cor(parked1), 2)
head(corr1[,1:8])
corrplot(corr1)

parked2 <- mydata[, c(5,11,12,13,15)]
corr2 <- round(cor(parked2), 2)
head(corr2[,1:6])
corrplot(corr2)