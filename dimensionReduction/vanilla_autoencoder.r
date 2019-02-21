####setup####
#reticulate::use_python('/usr/bin/python3')
library(keras)
K <- backend()

####parameters####
batch_size <- 128L
epochs <- 50L

img_rows <- 28L
img_cols <- 28L

originalDim <-img_rows *img_cols
latentDim <- img_rows *img_cols
encodingDim <- 32L

sampleN <- 10

####Model definition####
input_layer <- 
    layer_input(shape = c(originalDim) )
encoded <- 
    input_layer %>% 
    layer_dense(units = encodingDim, activation = "relu")
decoded <-
    encoded %>%
    layer_dense(units = latentDim, activation = "sigmoid")

####Encoder and Decoder definition####

# this model maps an input to its reconstruction
autoencoder <- keras_model(inputs = input_layer, outputs = decoded)
summary(autoencoder)

#Let's also create a separate encoder model:
# this model maps an input to its encoded representation
encoder <- keras_model(input_layer, encoded)

#As well as the decoder model:
# create a placeholder for an encoded (32-dimensional) input
encoded_input <- layer_input(shape = c(encodingDim))
# retrieve the last layer of the autoencoder model
decoder_layer = get_layer(autoencoder,index=3)
# create the decoder model
decoder <- keras_model(encoded_input, decoder_layer(encoded_input))


####compile and train the autoencoder####
autoencoder %>% compile(
    loss='mean_squared_error',
    optimizer='adam',
    metrics = c('binary_crossentropy')
)

##load data set and train####
mnist <- dataset_mnist()
x_train <- mnist$train$x
y_train <- mnist$train$y

x_test <- mnist$test$x
y_test <- mnist$test$y

preProcessing<-function(x, normalization=NULL, dimConcat=NULL){
    
    #dimConcat indicates the index dimension. dimConcat should be NULL or an integer. if dimConcat is not null, the array will be concatenated along this specified dimension
    
    
    if(normalization =="MaxNorm") {
        x<-x/ ( max(x,na.rm =TRUE) - min(x,na.rm =TRUE))
    }
    
    if(normalization =="minMaxNorm") {
        x<-x/ ( max(x,na.rm =TRUE) - min(x,na.rm =TRUE))
        }
    if(is.null(dimConcat)){
        x <- x %>% apply(dimAlongConcat, as.numeric) %>% t()
        }
    
    return(list(data=x,
                normalization = normalization,
                concatDim = concatDim))
}

reverseProcessing<-function(x,normalization=NULL,concatDim=TRUE){
    if(method =="minMaxNorm") x<-x/ ( max(x,na.rm =TRUE) - min(x,na.rm =TRUE))
    if(concatDim){
        x <- x %>% apply(1, as.numeric) %>% t()
    }
}


x_train <- x_train / 255
x_test <- x_test / 255

#concat dimension 
x_train <- x_train %>% apply(1, as.numeric) %>% t()
x_test <- x_test %>% apply(1, as.numeric) %>% t()

history <-
    autoencoder %>%
    keras::fit(x_train,
               x_train,
               epochs=epochs,
               shuffle=TRUE,
               batch_size = batch_size, 
               validation_data= list(x_test, x_test)
    )


#### Visualizations ####
library(ggplot2)
library(dplyr)
#Visualize the embedding
x_test_encoded <- 
    encoder %>% 
    predict(x = x_test)

x_test_encoded %>%
    as_data_frame() %>%
    mutate(class = as.factor(mnist$test$y)) %>%
    ggplot(aes(x = V1, y = V2, colour = class)) + geom_point()

#Visualize digits before and after applying encoder
mnist <- dataset_mnist()
x_train <- mnist$train$x
y_train <- mnist$train$y

# visualize the digits
par(mfcol=c(6,6))
par(mar=c(0, 0, 3, 0), xaxs='i', yaxs='i')
dim(x_train)
for (idx in 1:36) { 
    im <- x_train[idx,,]
    im <- t(apply(im, 2, rev)) 
    image(1:28, 1:28, im, col=gray((0:255)/255), 
          xaxt='n', main=paste(y_train[idx]))
}

encoderIm<-x_train[idx,,]
