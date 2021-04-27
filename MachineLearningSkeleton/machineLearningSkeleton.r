####Machine Learning Skeleton####

##required packages
# install.packages("caret")
# install.packages("Epi")
# install.packages("gbm")

##only in mac or linux
# install.packages("doMC")
# install.packages("parallel")

############################################################################################################
####Settings####

##set the working directory and output folder
#Windows
dataFolder <- ""
outputFolder <- ""
#Mac
dataFolder <- ""
outputFolder <- ""

fileName <- ""

##Set seed number
seedNum = 1234 #If you don't want to set seed, then let this be NULL. Otherwise put any number

##Set proportion for the test set
testProportion = 0.3

##set the outcome
outcome = "outcome" #The name of column
is.binary = TRUE #if the outcome is binary, please say it's TRUE, otherwise enter the FALSE

##Set number and metric for cross validation
crossValNum = 3
crossValMetric = "ROC" #ROC only for 'binary class', possible values are "RMSE" and "Rsquared" for regression and "Accuracy" and "Kappa" for classification

##set variables
varToExclude = NULL #if you have any variables to exclude in machine learning please set this. eg. c("HCMP_Diagnosis","Osteoporosis") , c(3,6,8)

#For parallelization to speed up!(only in Unix. You can use in Mac, too )
library(doMC)
library(parallel)
getDoParWorkers()
registerDoMC(cores = parallel::detectCores())
getDoParWorkers()
##########################################################################################################

##########################################################################################################
####Helper functions####
##Normalization of the elements in the data
minmaxNormalize <- function(x) {return ((x-min(x,na.rm = T)) / (max(x,na.rm = T)-min(x,na.rm = T)))}
zNormalize <- function(x) {return ( (x-mean(x,na.rm = T))/sd(x,na.rm = T))}
##########################################################################################################

####Data load and preprocessing####
##read the File
rawData <- read.csv(file.path(dataFolder,fileName), stringsAsFactors= FALSE)

##inspect data first
head(rawData)
str(rawData) #Check the data type of each column
summary(rawData)
length(rawData)
colnames(rawData)
nrow(rawData)

##data selection
outcomeIndex <- which(colnames(rawData)==outcome)

outcome = rawData[,outcomeIndex]

table(outcome)
prop.table(table(outcome)) * 100

#transformation of outcome into factor
if(class(outcome)=="integer"  ){
    if(is.binary) outcome = ifelse(outcome==max(outcome),"positive","negative")
}
outcome <- as.factor(outcome)

#exclude the excluding variables from the whole features
if(class(varToExclude)=="NULL"){
    feature = rawData[,-outcomeIndex]
} else {
    if(class(varToExclude)=="character"){
        varIndexToExclude = match(varToExclude,colnames(rawData))
    } else {
        if(class(varToExclude)=="numeric") varIndexToExclude = varToExclude
    }
    feature = rawData[,c(-outcomeIndex,-varIndexToExclude)]
}

##data normalization
feature <- as.data.frame(lapply(feature, minmaxNormalize))

##split train and test
set.seed(seedNum)
#testInd<-sample.int(n= nrow(wdbc_p), size = floor(testProportion* nrow(wdbc_p)), replace = F)
testInd<-caret::createDataPartition(outcome, p = testProportion, list = F) #To split total data set while preserving the proportion of outcomes in train and test set

featureTrain <- feature[-testInd,]
featureTest  <- feature[testInd,]

outcomeTrain <- outcome[-testInd]
outcomeTest <- outcome[testInd]


#Checking distibution in origanl data and partitioned data
prop.table(table(outcomeTrain)) * 100 ##you can see the degree of imbalance
prop.table(table(outcomeTest)) * 100

##set train control in caret package
if(is.binary){
    fitControl <- caret::trainControl(method = "repeatedcv", number = crossValNum, repeats = 5,
                                      classProbs=TRUE, summaryFunction = caret::twoClassSummary)
}else{
    fitControl <- caret::trainControl(method = "repeatedcv", number = crossValNum, repeats = 5,
                                      classProbs=TRUE) 
}

####KNN####
#1st method : use defulat setting
set.seed(seedNum)
trainFit <- caret::train(x=featureTrain,y=outcomeTrain, 
                    method = "knn", 
                    trControl = fitControl, 
                    #preProcess = c("center","scale"), 
                    metric = crossValMetric,
                    tuneLength = 3) #you can increase tune length to try more Ks
#Output of kNN fit
trainFit
#Plotting yields Number of Neighbours Vs accuracy (based on repeated cross validation)
plot(trainFit)

#2nd method : use custom K
set.seed(seedNum)
customGrid <- expand.grid(k = c(2,3))
trainFit <- caret::train(x=featureTrain,y=outcomeTrain, 
                         method = "knn", 
                         trControl = fitControl, 
                         #preProcess = c("center","scale"), 
                         metric = crossValMetric,# possible values are "RMSE" and "Rsquared" for regression and "Accuracy" and "Kappa" for classification
                         tuneGrid = customGrid)
#Output of kNN fit
trainFit
#Plotting yields Number of Neighbours Vs accuracy (based on repeated cross validation)
plot(trainFit)

############
####Performance of model####
#applying the trained model to the test set
predictClass <- predict(trainFit,newdata = featureTest)
predictProb <- predict(trainFit,newdata = featureTest,type ="prob")
#Get the confusion matrix to see accuracy value and other parameter values
caret::confusionMatrix(predictClass, outcomeTest,positive = "positive" )
#the accuracy
mean(predictClass == outcomeTest) 

#plot ROC curve
Epi::ROC(form=outcomeTest~predictProb[,2], plot="ROC")
out= Epi::ROC(form=outcomeTest~predictProb[,2], plot="ROC")
out
out$AUC
######################################################

####Linear regression####
set.seed(seedNum)
trainFit <- caret::train(x=featureTrain,y=outcomeTrain, 
                         method = "glm", 
                         trControl = fitControl, 
                         family = "binomial",
                         #preProcess = c("center","scale"), 
                         metric = crossValMetric,
                         tuneLength = 3) #you can increase tune length to try more Ks
#Output of kNN fit
trainFit

#check the importance of the variables
caret::varImp(trainFit)
plot(caret::varImp(trainFit))

####Performance of model####
#applying the trained model to the test set
predictClass <- predict(trainFit,newdata = featureTest)
predictProb <- predict(trainFit,newdata = featureTest,type ="prob")
#Get the confusion matrix to see accuracy value and other parameter values
caret::confusionMatrix(predictClass, outcomeTest,positive = "positive" )
#the accuracy
mean(predictClass == outcomeTest) 

#plot ROC curve
Epi::ROC(form=outcomeTest~predictProb[,2], plot="ROC")
out= Epi::ROC(form=outcomeTest~predictProb[,2], plot="ROC")
out$AUC

####Random Forest#####
#1st method: default optimal parameter searching
set.seed(seedNum)
trainFit <- caret::train(x=featureTrain,y=outcomeTrain, 
                         method = "rf", 
                         trControl = fitControl, 
                         #preProcess = c("center","scale"), 
                         metric = crossValMetric,
                         tuneLength = 10)## You can increase tuneLength
#Output of RF fit
trainFit
#Plotting yields Number of mtry Vs ROC (based on repeated cross validation)
plot(trainFit)
#check the importance of the variables
caret::varImp(trainFit)
plot(caret::varImp(trainFit))

#2nd method: searching the optimal parameter among user-defined grid(eg, mtry from 1 to 10)
customGrid <- expand.grid(mtry = 1:10)
set.seed(seedNum)
trainFit <- caret::train(x=featureTrain,y=outcomeTrain, 
                         method = "rf", 
                         trControl = fitControl, 
                         #preProcess = c("center","scale"), 
                         metric = crossValMetric,
                         tuneGrid = customGrid)
#Output of RF fit
trainFit
#Plotting yields Number of mtry Vs ROC (based on repeated cross validation)
plot(trainFit)

#3rd method: random search of hyper-parameter
set.seed(seedNum)
fitControlRandom <- caret::trainControl(method = "repeatedcv", number = crossValNum, repeats = 5,
                                  classProbs=TRUE, summaryFunction = twoClassSummary,
                                  search="random")
trainFit <- caret::train(x=featureTrain,y=outcomeTrain, 
                         method = "rf", 
                         trControl = fitControl, 
                         #preProcess = c("center","scale"), 
                         metric = crossValMetric,
                         tuneLength = 20)
#Output of RF fit
trainFit
#Plotting yields Number of mtry Vs ROC (based on repeated cross validation)
plot(trainFit)

############
####Performance of model####
#applying the trained model to the test set
predictClass <- predict(trainFit,newdata = featureTest)
predictProb <- predict(trainFit,newdata = featureTest,type ="prob")
#Get the confusion matrix to see accuracy value and other parameter values
caret::confusionMatrix(predictClass, outcomeTest,positive = "positive" )
#the accuracy
mean(predictClass == outcomeTest) 

#plot ROC curve
Epi::ROC(form=outcomeTest~predictProb[,2], plot="ROC")
out= Epi::ROC(form=outcomeTest~predictProb[,2], plot="ROC")
out
out$AUC
######################################################

####Gradient Boosting#####
set.seed(seedNum)
trainFit <- caret::train(x=featureTrain,y=outcomeTrain, 
                         method = "gbm", 
                         trControl = fitControl, 
                         #preProcess = c("center","scale"), 
                         metric = crossValMetric,
                         tuneLength = 10)## You can increase tuneLength
plot(trainFit)
############
####Performance of model####
#applying the trained model to the test set
predictClass <- predict(trainFit,newdata = featureTest)
predictProb <- predict(trainFit,newdata = featureTest,type ="prob")
#Get the confusion matrix to see accuracy value and other parameter values
caret::confusionMatrix(predictClass, outcomeTest,positive = "positive" )
#the accuracy
mean(predictClass == outcomeTest) 

#plot ROC curve
Epi::ROC(form=outcomeTest~predictProb[,2], plot="ROC")
out= Epi::ROC(form=outcomeTest~predictProb[,2], plot="ROC")
out
out$AUC
####################################################################################
####Deailng with class imbalance (trade-off between sensitivity and specificity#####
#library(purrr)
#library(DMwR)

####Weight in outcome
modelWeight <- ifelse(outcomeTrain =="yes", 1/table(outcomeTrain)[2]*0.5, 1/table(outcomeTrain)[1]*0.5)
set.seed(seedNum)
trainFit <- caret::train(x=featureTrain,y=outcomeTrain, 
                         method = "gbm", 
                         trControl = fitControl, 
                         #preProcess = c("center","scale"),
                         metric = crossValMetric,
                         weights = modelWeight,
                         tuneLength = 5)## You can increase tuneLength
plot(trainFit)
####Performance of model####
#applying the trained model to the test set
predictClass <- predict(trainFit,newdata = featureTest)
predictProb <- predict(trainFit,newdata = featureTest,type ="prob")
#Get the confusion matrix to see accuracy value and other parameter values
caret::confusionMatrix(predictClass, outcomeTest,positive = "positive" )

####Down-sampling (doesn't work in small data set)
fitControl$sampling <- "down"
set.seed(seedNum)
trainFit <- caret::train(x=featureTrain,y=outcomeTrain, 
                         method = "gbm", 
                         trControl = fitControl, 
                         #preProcess = c("center","scale"),
                         metric = crossValMetric,
                         tuneLength = 10)## You can increase tuneLength
plot(trainFit)
####Performance of model####
#applying the trained model to the test set
predictClass <- predict(trainFit,newdata = featureTest)
predictProb <- predict(trainFit,newdata = featureTest,type ="prob")
#Get the confusion matrix to see accuracy value and other parameter values
caret::confusionMatrix(predictClass, outcomeTest,positive = "positive" )

####Over(up)-sampling
fitControl$sampling <- "up"
set.seed(seedNum)
trainFit <- caret::train(x=featureTrain,y=outcomeTrain, 
                         method = "gbm", 
                         trControl = fitControl, 
                         #preProcess = c("center","scale"),
                         metric = crossValMetric,
                         tuneLength = 10)## You can increase tuneLength
plot(trainFit)
####Performance of model####
#applying the trained model to the test set
predictClass <- predict(trainFit,newdata = featureTest)
predictProb <- predict(trainFit,newdata = featureTest,type ="prob")
#Get the confusion matrix to see accuracy value and other parameter values
caret::confusionMatrix(predictClass, outcomeTest,positive = "positive" )

####Over(up)-sampling with SMOTE
fitControl$sampling <- "smote"
set.seed(seedNum)
trainFit <- caret::train(x=featureTrain,y=outcomeTrain, 
                         method = "gbm", 
                         trControl = fitControl, 
                         #preProcess = c("center","scale"),
                         metric = crossValMetric,
                         tuneLength = 10)## You can increase tuneLength
plot(trainFit)
####Performance of model####
#applying the trained model to the test set
predictClass <- predict(trainFit,newdata = featureTest)
predictProb <- predict(trainFit,newdata = featureTest,type ="prob")
#Get the confusion matrix to see accuracy value and other parameter values
caret::confusionMatrix(predictClass, outcomeTest,positive = "positive" )
