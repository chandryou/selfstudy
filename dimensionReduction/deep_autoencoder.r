#Source
#https://statslab.eighty20.co.za/posts/autoencoders_keras_r/

#set default python version for R
#reticulate::use_python("/usr/local/bin/python3")

# #install required packages
# install.packages("ggplot2")
# install.packages("tidyverse")
# install.packages("caret")
# install.packages("e1071")

library(tidyr)
library(dplyr)
library(keras)
library(ggplot2)

##set outputFolder
outputFolder <- "/Users/chan/git/chandryou/selfstudy/dimensionReduction"


##load and prepare the data
split_ind <- iris$Species %>% caret::createDataPartition(p = 0.8,list = FALSE)

train <- iris[split_ind,]
test <- iris[-split_ind,]

##preprocess
train_X <- train[,1:4] %>% as.matrix()

train_y <- as.numeric(train[,5]) %>% 
  keras::to_categorical()

test_X <- test[,1:4] %>% as.matrix()

##Define the encoder and decoder

input_layer <- 
  layer_input(shape = c(4)) 

encoder <- 
  input_layer %>% 
  layer_dense(units = 150, activation = "relu") %>% 
  layer_batch_normalization() %>% 
  layer_dropout(rate = 0.2) %>% 
  layer_dense(units = 50, activation = "relu") %>%
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 25, activation = "relu") %>%
  layer_dense(units = 2) # 2 dimensions for the output layer

decoder <- 
  encoder %>% 
  layer_dense(units = 150, activation = "relu") %>% 
  layer_dropout(rate = 0.2) %>% 
  layer_dense(units = 50, activation = "relu") %>%
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 25, activation = "relu") %>%
  layer_dense(units = 4) # 4 dimensions for the original 4 variables

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
  keras::fit(train_X,
             train_X,
             epochs=100,
             shuffle=TRUE,
             validation_data= list(test_X, test_X)
  )

#Visualize the embedding
reconstructed_points <- 
  autoencoder_model %>% 
  keras::predict_on_batch(x = train_X)

Viz_data <- 
  dplyr::bind_rows(
    reconstructed_points %>% 
      tibble::as_tibble() %>% 
      setNames(names(train_X %>% tibble::as_tibble())) %>% 
      dplyr::mutate(data_origin = "reconstructed"),
    train_X %>% 
      tibble::as_tibble() %>% 
      dplyr::mutate(data_origin = "original")
  )

Viz_data %>%
  ggplot(aes(Petal.Length,Sepal.Width, color = data_origin))+
  geom_point()


##Extract the weights for the encoder
autoencoder_weights <- 
  autoencoder_model %>%
  keras::get_weights()

str(autoencoder_weights)
##See the types fo variables of encoder
autoencoder_weights %>% purrr::map_chr(class)

##Save the weight
#keras::save_model_weights_hdf5(object = autoencoder_model,filepath = file.path(outputFolder,"autoencoder_weights.hdf5"),overwrite = TRUE)

##Load up the weights into an ecoder model and predict
encoder_model <- keras_model(inputs = input_layer, outputs = encoder)

encoder_model %>% keras::load_model_weights_hdf5(filepath = file.path(outputFolder,"autoencoder_weights.hdf5"),skip_mismatch = TRUE,by_name = TRUE)

encoder_model %>% compile(
  loss='mean_squared_error',
  optimizer='adam',
  metrics = c('accuracy')
)

####Compare to the PCA

##Autoencoder
embeded_points <- 
  encoder_model %>% 
  keras::predict_on_batch(x = train_X)

embeded_points %>% head

##PCA
pre_process <- caret::preProcess(train_X,method = "pca",pcaComp = 2)

pca <- predict(pre_process,train_X)

pca %>% head

Viz_data_encoded <- 
  dplyr::bind_rows(
    pca %>% 
      tibble::as_tibble() %>% 
      setNames(c("dim_1","dim_2")) %>% 
      dplyr::mutate(data_origin = "pca",
                    Species = train$Species),
    embeded_points %>% 
      tibble::as_tibble() %>% 
      setNames(c("dim_1","dim_2")) %>% 
      dplyr::mutate(data_origin = "embeded_points",
                    Species = train$Species)
  )

Viz_data_encoded %>% 
  ggplot(aes(dim_1,dim_2, color = data_origin))+
  geom_point()

Viz_data_encoded %>% 
  ggplot(aes(dim_1,dim_2, color = Species))+
  geom_point()+
  facet_wrap(~data_origin)

#Measure prediction accuracy
benchmark <- 
  Viz_data_encoded %>%
  mutate(Species = train$Species %>% rep(times = 2)) %>% 
  group_by(data_origin) %>% 
  nest() %>% 
  # mutate(model_lm = data %>% map(glm,formula = Species~., family = binomial())) %>% 
  # mutate(performance = model_lm %>% purrr::map(broom::augment)) %>% 
  # unnest(performance,.drop = FALSE)
  mutate(model_caret = data %>% purrr::map(~caret::train(form = Species~.,data = .x,method = "rf"))) 

benchmark

for(i in seq_along(benchmark$model_caret)){
  print(benchmark$data_origin[[i]])
  print(benchmark$model_caret[[i]])
}

