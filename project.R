## Load libraries
library(data.table)
library(caret)
library(ggplot2)
library(doMC)
library(knitr)
library(xtable)
library(randomForest)

## set seed
set.seed(13)

opts_chunk$set(echo = TRUE, results = 'hold')

## read training csv file and remove first column that contains ID row.
pml_train <- read.csv("/data/pml-training.csv", header=TRUE, sep=",", na.strings=c("NA",""))
pml_train <- pml_train[,-1]

## Create data partitions of training and validating data sets
inTrain = createDataPartition(pml_train$classe, p=0.60, list=FALSE)
training = pml_train[inTrain,]
validating = pml_train[-inTrain,]

## Data Exploration - # of columns with less than 60% of data
sum((colSums(!is.na(training[,-ncol(training)])) < 0.6*nrow(training)))

## Remove fields that doesn't have data prior to apply to model
Keep <- c((colSums(!is.na(training[,-ncol(training)])) >= 0.6*nrow(training)))
training <- training[,Keep]
validating <- validating[,Keep]

## Modeling - In random forests, not required for cross validation to
## get unbiased estimate of the test set error. It is estimated 
## internally during the run time. Proceed with training the model
## with training data set.
model <- randomForest(classe~.,data=training)
print(model)

## Model Evaluate
importance(model)

## Evaluate model results using confusion Matrix.
confusionMatrix(predict(model, newdata=validating[,-ncol(validating)]), validating$classe)

## Check Accuracy at validating data set by calculate it with formula
accuracy <- c(as.numeric(predict(model,newdata=validating[,-ncol(validating)]) == validating$classe))
accuracy <- sum(accuracy)*100/nrow(validating)

## Model Accuracy as tested over validating set = 99.8215%
print(accuracy)

## Model Test
## Proceed with predicting new values in testing csv file. Apply
## same cleaning operations etc
pml_test <- read.csv("/data/pml-testing.csv", header=TRUE, sep=",", na.strings=c("NA",""))
## Remove first column that is a ID row
pml_test <- pml_test[,-1]
## Keep same columns of testing dataset
pml_test <- pml_test[, Keep]
## Remove problem ID.
pml_test <- pml_test[,-ncol(pml_test)]

## Apply same transformations and Coerce Testing Dataset
testing <- rbind(training[100, -59], pml_test)
row.names(testing) <- c(100, 1:20)

## Predicting with Testing Dataset
predictions <- predict(model, newdata=testing[-1,])
print(predictions)

## Save Answers to file
pml_write_files = function(x) {
    n = length(x)
    for(i in 1:n) {
      filename = paste0("/data/problem_id_",i,".txt")
      write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}

pml_write_files(predictions)