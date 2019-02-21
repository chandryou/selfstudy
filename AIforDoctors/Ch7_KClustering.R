####Ch 7 of AI for Doctor####

##required packages
#install.packages("FastKNN")
#install.packages("gmodels")

##initial settings
DataFolder<-"~/data"
seedNumber<-3

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

##Run KNN
set.seed(seedNumber)
k_result<-stats::kmeans(wdbc_p,centers=2, iter.max = 10, algorithm = "Hartigan-Wong")

##Check the result
gmodels::CrossTable(k_result$cluster,wdbc$Class)
