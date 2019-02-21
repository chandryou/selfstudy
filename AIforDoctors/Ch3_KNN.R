####Ch 3 of AI for Doctor####

##required packages
#install.packages("FastKNN")
#install.packages("gmodels")

##initial settings
DataFolder<-"~/data"
seedNumber<-1

##read the File (pathology data from fine needle biopsy of breast tumor)
wdbc<-read.csv(file.path(DataFolder,"wdbc.csv"))

head(wdbc)
str(wdbc)
# nrow(wdbc)#569
table(wdbc$Class)

##data without IDs and labels
wdbc_p <- wdbc[,3:length(wdbc)]
#wdbc_p <- wdbc[,-c(1,2)] #the same wdbc_p with the code above. 

##Normalization of the elements in the data
minmaxNormalize <- function(x) {return ((x-min(x)) / max(x)-min(x))}
zNormalize <- function(x) {return ( (x-mean(x))/sd(x)  )}

wdbc_p <- as.data.frame(lapply(wdbc_p, minmaxNormalize))

##split train and test
testProportion<-0.4
set.seed(seedNumber)
testInd<-sample.int(n= nrow(wdbc_p), size = floor(testProportion* nrow(wdbc_p)), replace = F)

wdbc_train<-wdbc_p[-testInd,]
wdbc_test<-wdbc_p[testInd,]

wdbc_train_labels<-wdbc[-testInd,2]
wdbc_test_labels<-wdbc[testInd,2]

##KNN
wdbc_distance   <- FastKNN::Distance_for_KNN_test(wdbc_test, wdbc_train)
wdbc_prediction <- FastKNN::knn_test_function(wdbc_train,wdbc_test, wdbc_distance, wdbc_train_labels, k=11) 

##Write the table
result_table <- data.frame(wdbc[testInd,1], wdbc_test_labels, wdbc_prediction)
write.csv(result_table, file.path (DataFolder,"wdbc_result.csv"))

##check the accuracy
gmodels::CrossTable(wdbc_test_labels, wdbc_prediction)