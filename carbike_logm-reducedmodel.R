install.packages('aod')
install.packages('ggplot2')
install.packages('ISLR')
install.packages('dplyr')
install.packages('car')
vehicleData <- read.csv("G:/David/JCU/CITY OF SACREMENTO DATA/CSV/chunk4-vehicles.csv", header = T, sep = ",")
bicycleData <- read.csv("G:/David/JCU/CITY OF SACREMENTO DATA/CSV/chunk4-bicycles.csv", header = T, sep = ",")
#pedestrianData <- read.csv("G:/David/JCU/CITY OF SACREMENTO DATA/CSV/chunk4-pedestrians.csv", header = T, sep = ",")
#colnames(bicycleData)[11] <- "bike_class"  # rename response var
#colnames(bicycleData)[12] <- "bike_zone"  # rename response var
#vehicleData$bike_class <- bicycleData$bike_class
sapply(bicycleData, class)

vehicleData$is_parked <- as.logical(vehicleData$is_parked)
vehicleData$is_parked <- as.numeric(vehicleData$is_parked)
vehicleData$is_double_parked <- as.logical(vehicleData$is_double_parked)
vehicleData$is_double_parked <- as.numeric(vehicleData$is_double_parked)

vehicleData$trackedID <- as.numeric(vehicleData$trackedID)
vehicleData$frame_num <- as.numeric(vehicleData$frame_num)
vehicleData$bbox_xmin <- as.numeric(vehicleData$bbox_xmin)
vehicleData$bbox_xmax <- as.numeric(vehicleData$bbox_xmax)
vehicleData$bbox_ymin <- as.numeric(vehicleData$bbox_ymin)
vehicleData$bbox_ymax <- as.numeric(vehicleData$bbox_ymax)
vehicleData$num_objects_this_frame <- as.numeric(vehicleData$num_objects_this_frame)
vehicleData$total_objects_detected <- as.numeric(vehicleData$total_objects_detected)

bicycleData$trackedID <- as.numeric(bicycleData$trackedID)
bicycleData$frame_num <- as.numeric(bicycleData$frame_num)
bicycleData$bbox_xmin <- as.numeric(bicycleData$bbox_xmin)
bicycleData$bbox_xmax <- as.numeric(bicycleData$bbox_xmax)
bicycleData$bbox_ymin <- as.numeric(bicycleData$bbox_ymin)
bicycleData$bbox_ymax <- as.numeric(bicycleData$bbox_ymax)
bicycleData$num_objects_this_frame <- as.numeric(bicycleData$num_objects_this_frame)
bicycleData$total_objects_detected <- as.numeric(bicycleData$total_objects_detected)


#bikedat <- bicycleData[,11:12]

install.packages('dpylr')
library(dplyr)
carbikeData <- full_join(vehicleData, bicycleData, by = c('x','frame_num'), all = TRUE)
sapply(carbikeData, class)

set.seed(100) # set seed to replicate results
trainingIndex <- sample(1:nrow(carbikeData), 0.6*nrow(carbikeData)) # indices for 80% training data
trainingData <- carbikeData[trainingIndex, ] # training data
testData <- carbikeData[-trainingIndex, ] # test data

str(carbikeData)
summary(carbikeData)

#Logistic regression of the reduced model
logitMOD_J <- glm(is_double_parked ~ total_objects_detected.x + classes.x + dwell_time_secs + zone_name.y, data = carbikeData, family = binomial(link = 'logit'))
summary(logitMOD_J)
