#Download the files (if not already done)
if (!file.exists("pml-training.csv")) {
  url  = "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
  dest = "pml-training.csv"
  meth = "internal"
  quit = TRUE
  mode = "wb"
  download.file(url, dest, meth, quit, mode)
  #Works on tested operating system (Windows 7). Please change values if needed.
} 
if (!file.exists("pml-testing.csv")) {
  url  = "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
  dest = "pml-testing.csv"
  meth = "internal"
  quit = TRUE
  mode = "wb"
  download.file(url, dest, meth, quit, mode)
  #Works on tested operating system (Windows 7). Please change values if needed.
} 

#Load libraries
library(dplyr)
library(caret)
library(randomForest)

#Load the training and testing data
trainingdata <- read.csv("pml-training.csv", na.strings=c("NA","#DIV/0!",""))
testingdata <- read.csv("pml-testing.csv", na.strings=c("NA","#DIV/0!",""))

#Divide the training set in a training and testing set
inTrain <- createDataPartition(y=trainingdata$classe,
                               p=0.7, list=FALSE)
training <- trainingdata[inTrain,]
testing <- trainingdata[-inTrain,]

#Cleaning and transformations

#Removing the x variable (# observation)
training <- training[c(-1)]
testing <- testing[c(-1)]
testingdata <- testingdata[c(-1)]

#Removing variables with many NA's (treshold: 70% NA)
training2 <- data.frame(matrix(nrow=nrow(training)))
trainingnames <- character()
a <- numeric()

#Training set
for(i in 1:length(training)){
  if(sum(is.na(training[1:nrow(training), i]))/nrow(training) <= 0.3){
    training2 <- cbind(training2, training[,i])
    trainingnames <- append(trainingnames, names(training[i]))
    a <- append(a,i)
  }
}

training2 <- training2[,-1]
names(training2) <- trainingnames
training <- training2

#Apply the same transformation to the test set
test2 <- data.frame(matrix(nrow=nrow(testing)))
for(j in 1:length(a)){
  test2 <- cbind(test2, testing[,a[j]])
}

test2 <- test2[,-1]
names(test2) <- trainingnames
testing <- test2

#Apply the same transformation to the final test set
test3 <- data.frame(matrix(nrow=nrow(testingdata)))
for(k in 1:length(a)){
  test3 <- cbind(test3, testingdata[,a[k]])
}

test3 <- test3[,-1]
names(test3) <- trainingnames
colnames(test3)[59] <- names(testingdata[159])
testingdata <- test3

#Develop the model

#Run the random forest algoritm on the training set
model <- randomForest(classe ~. , data=(training))

#Test the model

#Run the model on the testing set (derived from the training data)
predictions <- predict(model, testing, type = "class")

#Check the accuracy of the model
confusionMatrix(predictions, testing$classe)


#Predict the test data

predictions2 <- predict(model, testingdata, type = "class")

pml_write_files = function(x){
  n = length(x)
  for(k in 1:n){
    filename = paste0("problem_id_",k,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(predictions2)

