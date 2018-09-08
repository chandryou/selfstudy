##Ch 4 of AI for Doctor : Decision Tree 
DataFolder<-"~/AIforDoctor/Ch4"
seedNumber<-1

#install.packages('C50', dependencies = T)
library(C50)

##################################################################
#################prediction of UTI################################

#read the File (pathology data from fine needle biopsy of breast tumor)
UTI<-read.csv(file.path(DataFolder,"UTI.csv"))
str(UTI)

#remove patientId
UTI<-UTI[,-1]

#split train and test
testProportion<-0.2
set.seed(seedNumber)
testInd<-sample.int(n= nrow(UTI), size = floor(testProportion* nrow(UTI)), replace = F)

UTI_train<-UTI[-testInd,-length(UTI)]
UTI_test<-UTI[testInd,-length(UTI)]

UTI_train_labels<-UTI[-testInd,length(UTI)]
UTI_test_labels<-UTI[testInd,length(UTI)]

#Building tree model and prediction
UTI_model<-C50::C5.0(UTI_train,UTI_train_labels)
UTI_prediction <- C50::predict.C5.0(UTI_model, UTI_test)

#Evaluation of the model
plot(UTI_model)
summary(UTI_model)
gmodels::CrossTable(UTI_prediction, UTI_test_labels)
a1=Epi::ROC(form=UTI_test_labels~UTI_prediction,plot="ROC")

##################################################################
##################################################################

##################################################################
#################prediction of dermatology########################
dermatology<- read.csv(file.path(DataFolder,"dermatology.csv"))
str(dermatology)

#split train and test
testProportion<-0.3
set.seed(seedNumber)
testInd<-sample.int(n= nrow(dermatology), size = floor(testProportion* nrow(dermatology)), replace = F)

derma_train<-dermatology[-testInd,-length(dermatology)]
derma_test<-dermatology[testInd,-length(dermatology)]

derma_train_labels<-dermatology[-testInd,length(dermatology)]
derma_test_labels<-dermatology[testInd,length(dermatology)]

derma_model<- C50::C5.0(derma_train,derma_train_labels)
derma_pred <- C50::predict.C5.0(derma_model,derma_test)

plot(derma_model)
gmodels::CrossTable(derma_pred, derma_test_labels)
a1=Epi::ROC(form=derma_test_labels~derma_pred,plot="ROC")
