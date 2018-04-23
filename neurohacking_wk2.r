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
library(oro.nifti)
setwd(file.path(dataFolder,"BRAINIX/NIfTI"))
fname = "Output_3D_File"
print({niiT1 = oro.nifti::readNIfTI(fname = fname)})

#Slices
nii_T1= oro.nifti::readNIfTI(fname = fname)
d=dim(nii_T1)
graphics::image(1:d[1], 1:d[2], nii_T1[,,11], xlab = "", ylab= "") #heat color

oro.nifti::image(nii_T1, z= 11, plot.type = "single") #image of nifii object

oro.nifti::image(nii_T1) #Visualizing all slices (default by axial)

oro.nifti::orthographic (nii_T1, xyz = c(200,220,11)) ##All planes : Coronal, Sagittal, Axial

#Histograms
par(mfrow=c(1,2))
o<-par(mar=c(4,4,0,0))
hist(nii_T1, breaks = 75, prob = T, xlab = "T1 intensities", col = rgb(0,0,1,1/2),main = "")
hist(nii_T1[nii_T1>20], breaks = 75, prob = T, xlab= "T1 intensities > 20", col = rgb(0,0,1,1/2), main = "")

#Back mapping

is_btw_300_400 <- ( (nii_T1>300) & (nii_T1<400))
nii_T1_mask <- nii_T1
nii_T1_mask[ !is_btw_300_400 ] = NA
oro.nifti::overlay(nii_T1, nii_T1_mask, z = 11, plot.type = "single") #reds are white matter
oro.nifti::overlay(nii_T1, nii_T1_mask)
oro.nifti::orthographic(nii_T1, nii_T1_mask, xyz = c(200, 220, 11),
                        text = "Image overlaid with mask", text.cex = 1.5)
    
###Functions Discussed

oro.nifti::readNIfTI() #read in the data

graphics::image() #display matrix of data

oro.nifti::image() #display NIfTI data

oro.nifti::readNIfTI() #display 3-planes of an image

oro.nifti::overlay() #display overlay of two images, NAs are not plotted in the y-image

##############################################

###############################################
##2.4 Basic Data Manipulation##################

mridir<-file.path(dataFolder,"Kirby21/visit_1/113/")
T1<-oro.nifti::readNIfTI(file.path(mridir,"/113-01-MPRAGE.nii.gz"))
oro.nifti::orthographic(T1)

mask<-oro.nifti::readNIfTI(file.path(mridir, '113-01-MPRAGE_mask.nii.gz'), reorient = FALSE)
orthographic(mask)

#Masking
masked.T1<-T1*mask
orthographic(masked.T1)

#Operations (addition, subtraction, maultiplication)

T1.follow<-oro.nifti::readNIfTI(file.path(dataFolder,"Kirby21/visit_2/113/","/113-02-MPRAGE.nii.gz"))
subtract.T1<-T1.follow - T1
min(subtract.T1)
orthographic(subtract.T1)
###############################################

####################################################
##2.5 Transformation and Smoothing##################

par (new = TRUE)
curve(x*1, axes =FALSE, xlab = "", ylab = "", col = 2, lwd = 3)
im_hist<-hist(nii_T1)
axis(side = 4, at = pretty (range(im_hist$mids))/
         max(nii_T1), labels = pretty(range(im_hist$mids)))
mtext("original intensity", side = 4, line = 2)

#linear spline transformation
lin.sp<-function(x,knots,slope){
    knots <- c(min(x), knots, max(x))
    slopeS<-slope[1]
    for (j in 2:length(slope)){
        slopeS<-c(slopeS,slope[j] - sum(slopeS))
    }
    
    rvals<-numeric(length(x))
    for ( i in 2:length(knots)){
        rvals <- ifelse(x>= knots[i-1], slopeS[i-1]*(x-knots[i-1]) + rvals, rvals)
    }
        return(rvals)
}

#Define a spline with two knots and three slopes
knots.vals<-c(.3,.6)
slp.vals <- c(1, .5, .25)

par (new = TRUE)
curve(lin.sp(x, knots.vals, slp.vals), axes =FALSE, xlab = "", ylab = "", col = 2, lwd = 3)
axis(side = 4, at = pretty (range(im_hist$mids))/
         max(nii_T1), labels = pretty(range(im_hist$mids)))
mtext("Transformed Intensity", side = 4, line = 2)

trans_T1<-lin.sp(nii_T1, knots.vals*max(T1), slp.vals)
image(nii_T1,z=15, plot.type = 'single', main = "Original Image")
image(trans_T1,z=15, plot.type = 'single', main = "Transformed Image")

##Smoothing with a Gaussian smoother
smooth.T1<-AnalyzeFMRI::GaussSmoothArray(nii_T1, voxdim = c(1,1,1),
                                         ksize = 11, sigma = diag(3,3),
                                         mask = NULL,
                                         var.norm =FALSE)
orthographic(smooth.T1)
####################################################

####################################################
##2.6 Basic MRI Contrasts###########################


##FLAIR
mridir<-file.path(dataFolder,"BRAINIX/NIfTI")
sequence <- "FLAIR"

volume.f<-readNIfTI(file.path(mridir,paste0(sequence,".nii.gz")), reorient=FALSE)
volume.f <- cal_img(volume.f)
image(volume.f, z = 12,
      plot.type="single")

####################################################

####################################################
##