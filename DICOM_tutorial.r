install.packages("oro.dicom")
library(oro.dicom)
fname<-system.file(file.path("/Users/chan/data/ASPECTS/images/120001 KJY/D0 CT", "I0043483.dcm"),package="oro.dicom")
abdo <- readDICOMFile(fname)

fname<-"/Users/chan/data/ASPECTS/images/120001 KJY/D0 CT/I0043483.dcm"
kjy.ct<-readDICOMFile(fname)
#show the image in the R studio
image(t(kjy.ct$img), col=grey(0:64/64), axes=FALSE, xlab="", ylab="")
dput(formals(dicom2nifti))
kjy.ct.n<-dicom2nifti(kjy.ct, DIM=2)

extractHeader(kjy.ct$hdr, "Manufacturer", numeric =FALSE) #"SIEMENS"
extractHeader(kjy.ct$hdr, "RepetitionTime")
extractHeader(kjy.ct$hdr, "EchoTime")
dim(kjy.ct$img) #512 512

#make 3 dimensional nifti
fname<-"/Users/chan/data/ASPECTS/extra"
images<-readDICOM(fname)
imagesn<-dicom2nifti(images)

#making 4 dimensional nifti
images.4d<-dicom2nifti(images,DIM=4)
