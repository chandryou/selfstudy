##WEEK 2##
#library(oro.dicom)
#library(oro.nifti)

dataFolder<-"/Users/chan/git/Neurohacking_data"

#################################################
##2.1 Data Structures and Operations#############

##Loading one DICOM file########################
setwd(file.path(dataFolder,"BRAINIX/DICOM/FLAIR"))
slice=oro.dicom::readDICOM("IM-0001-0011.dcm")
class(slice)
#Output is a list with two elements that are themselves lists(1) the DICOM header (hdr)
#and (2) the image (img)
#Each element of hdr is a data.frame and each element of img is a matrix

names(slice)
class(slice$hdr)
class(slice$hdr[[1]])
class(slice$img[[1]])
dim(slice$img[[1]])

d=dim(t(slice$img[[1]]))
image(1:d[1], 1:d[2], t(slice$img[[1]]), col=gray(0:64/64))

#histogram of FLAIR image
hist(slice$img[[1]][,],
     breaks = 50, xlab = "FLAIR",
     prob = T, col=rgb(0,0,1,1/4), main="")

##DICOM header information
hdr = slice$hdr[[1]]
names(hdr)

hdr$name #162

hdr[hdr$name =="PixelSpacing", "value"]
hdr[hdr$name =="FlipAngle",]

##Loading multiple DICOM files########
setwd(file.path(dataFolder,"BRAINIX/DICOM"))
all_slices_T1 = oro.dicom::readDICOM("T1/")

dim(all_slices_T1$img[[11]])

hdr = all_slices_T1$hdr[[11]]
hdr[hdr$name == "PixelSpacing", "value"] #"0.46875 0.46875"
#########################################

##############################################
##2.2 The NIfTI format########################

"
NIfTI (Neuroimaging Informatics Technology Initiative) format
-Standardized representation of images
-Most commonly used type of analytic file
-Developed to facilitate cross-platform, cross-software interpretability
-3-Dimensional (3D) array: stacking individual slices on top of each other
-DICOM: one sheet of paper, NIfTI: stack of papers
"

#From DICOM to NIfTI
setwd(file.path(dataFolder,"BRAINIX/DICOM"))
all_slices_T1 = oro.dicom::readDICOM("T1/")

nii_T1 = oro.dicom::dicom2nifti(all_slices_T1)
d=dim(nii_T1)
d #512 512  22
class(nii_T1) #nifti
image(1:d[1],1:d[2],nii_T1[,,11],col=gray(0:64/64),
      xlab="",ylab="")

#Write and Read NIfTI files
setwd(file.path(dataFolder,"BRAINIX/NIfTI"))
fname = "Output_3D_File"
oro.nifti::writeNIfTI(nim=nii_T1,filename=fname)

list.files(getwd(), pattern = "Output_3D_File")
list.files(getwd(), pattern = "T")

nii_T2 = oro.nifti::readNIfTI("T2.nii.gz",reorient=FALSE)
dim(nii_T2)

#Compressed Image Files
"
Compressed Image Files
-Files are in compressed format with the extension .nii.gz
-Saves disk space, makes read/write data very fast
-Excellent for scripting, analysis of image population
-A non-comprssed file can be obtained using the argument gzipped=FALSE in the function 'writeNIfTI'
-No extension for file name in 'writeNIfTI'
"

"If you don't like R you can use the 'dcm2nii' software as part of the MRIcron platform "

##############################################

##############################################
##2.3 Basic Visualization#####################

##############################################

###############################################
##2.4 Basic Data Manipulation##################

###############################################


####################################################
##2.5 Transformation and Smoothing##################

####################################################

####################################################
##2.6 Basic MRI Contrasts###########################

####################################################