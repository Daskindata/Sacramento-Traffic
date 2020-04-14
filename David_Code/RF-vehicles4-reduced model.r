install.packages("disk.frame", repo="https://cran.rstudio.com/")

library(forcats)
#install.packages('dplyr')
library(MASS)
library(randomForest)
#library(caret)
library(reticulate)
#library(dplyr)
sagemaker <- import('sagemaker')

session <- sagemaker$Session()
bucket <- session$default_bucket()

inputData = read.csv(file = 'chunk4-vehicles.csv', header = TRUE, sep = ',')

summary(inputData)

inputData$trackedID <- as.numeric(inputData$trackedID)
inputData$frame_num <- as.numeric(inputData$frame_num)
inputData$bbox_xmin <- as.numeric(inputData$bbox_xmin)
inputData$bbox_xmax <- as.numeric(inputData$bbox_xmax)
inputData$bbox_ymin <- as.numeric(inputData$bbox_ymin)
inputData$bbox_ymax <- as.numeric(inputData$bbox_ymax)
inputData$num_objects_this_frame <- as.numeric(inputData$num_objects_this_frame)
inputData$total_objects_detected <- as.numeric(inputData$total_objects_detected)

#inputData$is_parked <- as.logical(inputData$is_parked)
#inputData$is_parked <- as.numeric(inputData$is_parked)
#inputData$is_double_parked <- as.logical(inputData$is_double_parked)
#inputData$is_double_parked <- as.numeric(inputData$is_double_parked)

set.seed(100) # set seed to replicate results
trainingIndex <- sample(1:nrow(inputData), 0.5*nrow(inputData)) # indices for 60% training data
trainingData <- inputData[trainingIndex, ] # training data
testData <- inputData[-trainingIndex, ] # test data

#trainingData_RF <- trainingData[, -c(1,2,3,5,6,7,8,9,10,11)]
#trainingData_RF$zone_name <- levels(droplevels(trainingData_RF$zone_name))
summary(trainingData)
summary(testData)
#str(trainingData)
str(testData)
#summary(trainingData_RF)
#str(trainingData_RF)

set.seed(1234)# can be any number 
#trainingData_RF <- droplevels(trainingData_RF)
#trainingData_RF$zone_name <- droplevels(trainingData_RF$zone_name)
#rf1 <- randomForest(is_double_parked ~ ., trainingData[1:883459,], importance=TRUE)
#rf2 <- randomForest(zone_name ~ ., trainingData[883460:1766917,], importance=TRUE)
#rf.combined <- combine(rf1,rf2)
#rf.combined
parked <- trainingData[1:800000,]
parked <- parked[-c(1,2,3,4,6,7,8,9,10)]
rf.model1 <- randomForest(is_double_parked ~., data = parked, na.action = na.omit, proximity=FALSE, importance = TRUE)
rf.model1

# Fine tuning parameters of Random Forest model
rf.model2 <- randomForest(is_double_parked ~ ., data = parked, ntree = 500, mtry = 6, proximity = FALSE, importance = TRUE, na.action = na.omit)
rf.model2

parktest <- testData[1:340000,]
parktest <- parktest[-c(1,2,3,4,6,7,8,9,10)]
trainset = parked
#testset <- testData[1:340000,]
predtrain = predict(rf.model2, trainset, type = "class")
table(predtrain, trainset$is_double_parked)

str(trainset)
str(parktest)

#trainset = trainingData[1:800000,]
#testset <- testData[1:340000,]
predvalid <- predict(rf.model2, parktest, type = "class")
mean(predvalid == parktest$is_double_parked)
table(predvalid, parktest$is_double_parked)

importance(rf.model2)        
varImpPlot(rf.model2)  

# Using For loop to identify the right mtry for model
a=c()
i=5
for (i in 3:8) {
  model3 <- randomForest(is_double_parked ~ ., data = trainset, ntree = 500, mtry = i, importance = TRUE)
  predValid <- predict(model3, parktest, type = "class")
  a[i-2] = mean(predValid == parktest$is_double_parked)
}
 
a
 
plot(3:8,a)


