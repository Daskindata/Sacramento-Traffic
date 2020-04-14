install.packages('aod')
install.packages('ggplot2')
install.packages('ISLR')
install.packages('dplyr')
install.packages('car')
inputData <- read.csv("G:/David/JCU/CITY OF SACREMENTO DATA/CSV/chunk4-vehicles.csv", header = T, sep = ",")
head(inputData)
summary(inputData)

#convert integer to numeric
inputData$trackedID <- as.numeric(inputData$trackedID)
inputData$frame_num <- as.numeric(inputData$frame_num)
inputData$bbox_xmin <- as.numeric(inputData$bbox_xmin)
inputData$bbox_xmax <- as.numeric(inputData$bbox_xmax)
inputData$bbox_ymin <- as.numeric(inputData$bbox_ymin)
inputData$bbox_ymax <- as.numeric(inputData$bbox_ymax)
inputData$num_objects_this_frame <- as.numeric(inputData$num_objects_this_frame)
inputData$total_objects_detected <- as.numeric(inputData$total_objects_detected)

#convert True-False to 1-0
inputData$is_parked <- as.logical(inputData$is_parked)
inputData$is_parked <- as.numeric(inputData$is_parked)
inputData$is_double_parked <- as.logical(inputData$is_double_parked)
inputData$is_double_parked <- as.numeric(inputData$is_double_parked)
head(inputData)

# Create Training Data
set.seed(100) # set seed to replicate results
trainingIndex <- sample(1:nrow(inputData), 0.6*nrow(inputData)) # indices for 60% training data
trainingData <- inputData[trainingIndex, ] # training data
testData <- inputData[-trainingIndex, ] # test data

#trainingData_RF <- trainingData[, -c(1,2,3,5,6,7,8,9,10,11)]
#trainingData_RF$zone_name <- levels(droplevels(trainingData_RF$zone_name))
summary(trainingData)
str(trainingData)
str(testData)
#summary(trainingData_RF)
#str(trainingData_RF)

#trainingData$zone_name <- factor(trainingData$zone_name)
#trainingData$classes <- factor(trainingData$classes)
logitMOD <- glm(is_double_parked ~ zone_name + classes + dwell_time_secs, data = trainingData, family = binomial(link = 'logit'))
summary(logitMOD)

logitMOD2 <- glm(is_double_parked ~., data = trainingData, family = binomial(link = 'logit'))
summary(logitMOD2)

logitMOD3 <- glm(is_double_parked ~ zone_name + num_objects_this_frame + classes + dwell_time_secs, data = trainingData, family = binomial(link = 'logit'))
summary(logitMOD3)

logitMOD4 <- glm(is_double_parked ~ total_objects_detected + dwell_time_secs + is_parked , family = 'poisson', data = trainingData)
summary(logitMOD4)

logitMOD5 <- glm(is_double_parked ~ total_objects_detected + is_parked + dwell_time_secs, data = trainingData, family = binomial(link = 'logit'))
summary(logitMOD5)

