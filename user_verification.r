# R script to implement user verification based on smartphone data 
# Author: Shivanand Venkanna Sheshappanavar
# Email: ssheshap@syr.edu

library(caret) #
library(randomForest) # random forest
library(class)  # knn
library(plyr)

partition_dataset <- function(newdata, name) {
  inTrain <- createDataPartition(y=newdata$user, p=0.7, list=FALSE)
  train <- newdata[inTrain, ]
  test <- newdata[-inTrain, ]
  train_file <- paste('./CleanData/', name, '_train.RData', sep="")
  test_file <- paste('./CleanData/', name, '_test.RData', sep="")
  save(train, file=train_file)
  save(test, file=test_file)
}

# Load .RData file into environment.
loadRData <- function(fileName){
  load(fileName)
  get(ls()[ls() != "fileName"])
}

# Execute the user verification
run <- function() {
  analysis("sitting")
}

# Execute classification on a target data set.
analysis <- function(name) {
  train_file <- paste('./CleanData/', name, '_train.RData', sep="")
  test_file <- paste('./CleanData/', name, '_test.RData', sep="")
  print("train and test ready to load")
  train1 <<- loadRData(train_file)
  test1 <<- loadRData(test_file)
  train <- na.omit(train1)
  test <<- na.omit(test1)
  random_forest(train,test)
}

random_forest <- function(train,test) {
  model <- randomForest(user ~ ., data=train)
  cross_val <- predict(model, test)
  rf_ret <<- c(rf_ret, confusionMatrix(cross_val, test$user))
}

load('data_104users.RData') #data set is confidential and not shared with code. 
set.seed(100)
label_index <- c(1,5,9:11,15:26,33:35,131:134) #feature column list
data1 <- data_104users[, label_index] #using only those feature columns

sitting <- subset(data1, activity_labels == 0)
sitting$activity_labels <- NULL

# The list to store classification performance of the classifier.
rf_ret <- NULL    # random forest
N<-104
i<-1 # i indicates user
while (i<=N) {
  user1 <- subset(sitting, user == i)
  otherusers <- sitting[sample(which(sitting$user != i), nrow(user1)),]
  otherusers$user <- as.integer(0)
  newdata1 <- rbind(user1,otherusers)
  newdata <- na.omit(newdata1)
  newdata$user <- as.factor(newdata$user)
  partition_dataset(newdata, "sitting")

  train <- NULL
  test <- NULL
  run()
  save(knn_ret,   file="./CleanData/ret/knn_ret.RData")
  save(rf_ret,    file="./CleanData/ret/rf_ret.RData")
  i <- i+1
}

