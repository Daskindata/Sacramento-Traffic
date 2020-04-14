install.packages('car')
install.packages('ridge')
install.packages('dplyr')
#load data
inputData <- read.csv("G:/David/JCU/CITY OF SACREMENTO DATA/CSV/chunk4-vehicles.csv", header = T, sep = ",")
class(inputData)
sapply(inputData, class)

# covert integer to numeric
inputData$trackedID <- as.numeric(inputData$trackedID)
inputData$frame_num <- as.numeric(inputData$frame_num)
inputData$bbox_xmin <- as.numeric(inputData$bbox_xmin)
inputData$bbox_xmax <- as.numeric(inputData$bbox_xmax)
inputData$bbox_ymin <- as.numeric(inputData$bbox_ymin)
inputData$bbox_ymax <- as.numeric(inputData$bbox_ymax)
inputData$num_objects_this_frame <- as.numeric(inputData$num_objects_this_frame)
inputData$total_objects_detected <- as.numeric(inputData$total_objects_detected)

#convert factor to numeric
inputData$is_parked <- as.logical(inputData$is_parked)
inputData$is_parked <- as.numeric(inputData$is_parked)
inputData$is_double_parked <- as.logical(inputData$is_double_parked)
inputData$is_double_parked <- as.numeric(inputData$is_double_parked)
#inputData[,'is_double_parked'] <- lapply(inputData[,'is_double_parked'], as.numeric)
head(inputData)
sapply(inputData, class)

#split data
set.seed(100) # set seed to replicate results
trainingIndex <- sample(1:nrow(inputData), 0.7*nrow(inputData)) # indices for 70% training data
trainingData <- inputData[trainingIndex, ] # training data
testData <- inputData[-trainingIndex, ] # test data

library('car')
lmMod <- lm(is_double_parked ~ ., trainingData)  # the linear reg model
summary(lmMod) # get summary

lmMod2 <- lm(is_double_parked ~ ., data = trainingData[-c(1,2,3,4,6,7,8,9,10,14)])  # the linear reg model
summary(lmMod2) # get summary
