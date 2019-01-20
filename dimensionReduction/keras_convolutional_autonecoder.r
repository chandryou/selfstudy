library(keras)

batch_size <- 128
img_rows <- 28
img_cols <- 28

mnist <- dataset_mnist()
x_train <- mnist$train$x
y_train <- mnist$train$y
x_test <- mnist$test$x
y_test <- mnist$test$y

dim(x_train) <- c(nrow(x_train), img_rows, img_cols, 1) 
dim(x_test) <- c(nrow(x_test), img_rows, img_cols, 1)
input_dim <- c(img_rows, img_cols, 1)
input_dim

x_train <- x_train / 255
x_test <- x_test / 255

### define model
CAE <- keras_model_sequential()
CAE %>%
  layer_conv_2d(filters = 16, kernel_size = c(3,3), activation = 'relu',padding='same', input_shape = input_dim)%>% 
  layer_max_pooling_2d(pool_size = c(2, 2),padding='same')%>% 
  layer_conv_2d(filters = 8, kernel_size = c(3,3), activation = 'relu',padding='same')%>% 
  layer_max_pooling_2d(pool_size = c(2, 2),padding='same')%>% 
  layer_conv_2d(filters = 8, kernel_size = c(3,3), activation = 'relu',padding='same')%>% 
  layer_max_pooling_2d(pool_size = c(2, 2),padding='same')%>% 
  layer_conv_2d(filters = 8, kernel_size = c(3,3), activation = 'relu',padding='same')%>% 
  layer_upsampling_2d(size = c(2, 2))%>%
  layer_conv_2d(filters = 8, kernel_size = c(3,3), activation = 'relu',padding='same')%>% 
  layer_upsampling_2d(size = c(2, 2))%>%
  layer_conv_2d(filters = 16, kernel_size = c(3,3), activation = 'relu')%>% 
  layer_upsampling_2d(size = c(2, 2))%>%
  layer_conv_2d(filters = 1, kernel_size = c(3,3), activation = 'sigmoid',padding='same') 


summary(CAE)

### compile model
CAE %>% compile(optimizer='adadelta', loss='binary_crossentropy')

CAE %>% fit(
  x_train, x_train,
  epochs = 50,
  batch_size = 128,
  shuffle = TRUE,
  validation_data = list(x_test, x_test)
)

"
input_layer <- 
  layer_input(shape = input_dim) 

encoderv<-
layer_input %>%
layer_conv_2d(filters = 16, kernel_size = c(3,3), activation = 'relu',padding='same')%>% 
layer_max_pooling_2d(pool_size = c(2, 2),padding='same')%>% 
layer_conv_2d(filters = 8, kernel_size = c(3,3), activation = 'relu',padding='same')%>% 
layer_max_pooling_2d(pool_size = c(2, 2),padding='same')%>% 
layer_conv_2d(filters = 8, kernel_size = c(3,3), activation = 'relu',padding='same')%>% 
layer_max_pooling_2d(pool_size = c(2, 2),padding='same')

decoder <-
encoder %>%
layer_upsampling_2d(size = c(2, 2))%>%
layer_conv_2d(filters = 8, kernel_size = c(3,3), activation = 'relu',padding='same')%>% 
layer_upsampling_2d(size = c(2, 2))%>%
layer_conv_2d(filters = 16, kernel_size = c(3,3), activation = 'relu')%>% 
layer_upsampling_2d(size = c(2, 2))%>%
layer_conv_2d(filters = 1, kernel_size = c(3,3), activation = 'sigmoid',padding='same') 

##compile and train the autoencoder
convAutoencoder_model <- keras_model(inputs = input_layer, outputs = decoder)

autoencoder_model %>% compile(
loss='mean_squared_error',
optimizer='adam',
metrics = c('accuracy')
)
summary(autoencoder_model)

##train onto itself
history <-
autoencoder_model %>%
keras::fit(train_X,
train_X,
epochs=100,
shuffle=TRUE,
validation_data= list(test_X, test_X)
)
"