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

x_train <- x_train / 255
x_test <- x_test / 255

input_layer <- 
    layer_input(shape = input_dim) 
encoder <- 
    input_layer %>% 
    layer_dense(units = 64, activation = "sigmoid")
decoder <-
    encoder %>%
    layer_dense(units = input_dim)

##compile and train the autoencoder
autoencoder_model <- keras_model(inputs = input_layer, outputs = decoder)

autoencoder_model %>% compile(
    loss='mean_squared_error',
    optimizer='adam',
    metrics = c('accuracy')
)

summary(autoencoder_model)

##train onto itself
history <-
    autoencoder_model %>%
    keras::fit(x_train,
               x_train,
               epochs=10,
               shuffle=TRUE,
               validation_data= list(x_test, x_test)
    )

