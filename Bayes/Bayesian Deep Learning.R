#https://blogs.rstudio.com/tensorflow/posts/2018-11-12-uncertainty_estimates_dropout/
library(keras)
library(ggplot2)

######################################
####A wrapper for learning dropout####
######################################

# R6 wrapper class, a subclass of KerasWrapper
ConcreteDropout <- R6::R6Class("ConcreteDropout",
                               
                               inherit = KerasWrapper,
                               
                               public = list(
                                   weight_regularizer = NULL,
                                   dropout_regularizer = NULL,
                                   init_min = NULL,
                                   init_max = NULL,
                                   is_mc_dropout = NULL,
                                   supports_masking = TRUE,
                                   p_logit = NULL,
                                   p = NULL,
                                   
                                   initialize = function(weight_regularizer,
                                                         dropout_regularizer,
                                                         init_min,
                                                         init_max,
                                                         is_mc_dropout) {
                                       self$weight_regularizer <- weight_regularizer
                                       self$dropout_regularizer <- dropout_regularizer
                                       self$is_mc_dropout <- is_mc_dropout
                                       self$init_min <- k_log(init_min) - k_log(1 - init_min)
                                       self$init_max <- k_log(init_max) - k_log(1 - init_max)
                                   },
                                   
                                   build = function(input_shape) {
                                       super$build(input_shape)
                                       
                                       self$p_logit <- super$add_weight(
                                           name = "p_logit",
                                           shape = shape(1),
                                           initializer = initializer_random_uniform(self$init_min, self$init_max),
                                           trainable = TRUE
                                       )
                                       
                                       self$p <- k_sigmoid(self$p_logit)
                                       
                                       input_dim <- input_shape[[2]]
                                       
                                       weight <- private$py_wrapper$layer$kernel
                                       
                                       kernel_regularizer <- self$weight_regularizer * 
                                           k_sum(k_square(weight)) / 
                                           (1 - self$p)
                                       
                                       dropout_regularizer <- self$p * k_log(self$p)
                                       dropout_regularizer <- dropout_regularizer +  
                                           (1 - self$p) * k_log(1 - self$p)
                                       dropout_regularizer <- dropout_regularizer * 
                                           self$dropout_regularizer * 
                                           k_cast(input_dim, k_floatx())
                                       
                                       regularizer <- k_sum(kernel_regularizer + dropout_regularizer)
                                       super$add_loss(regularizer)
                                   },
                                   
                                   concrete_dropout = function(x) {
                                       eps <- k_cast_to_floatx(k_epsilon())
                                       temp <- 0.1
                                       
                                       unif_noise <- k_random_uniform(shape = k_shape(x))
                                       
                                       drop_prob <- k_log(self$p + eps) - 
                                           k_log(1 - self$p + eps) + 
                                           k_log(unif_noise + eps) - 
                                           k_log(1 - unif_noise + eps)
                                       drop_prob <- k_sigmoid(drop_prob / temp)
                                       
                                       random_tensor <- 1 - drop_prob
                                       
                                       retain_prob <- 1 - self$p
                                       x <- x * random_tensor
                                       x <- x / retain_prob
                                       x
                                   },
                                   
                                   call = function(x, mask = NULL, training = NULL) {
                                       if (self$is_mc_dropout) {
                                           super$call(self$concrete_dropout(x))
                                       } else {
                                           k_in_train_phase(
                                               function()
                                                   super$call(self$concrete_dropout(x)),
                                               super$call(x),
                                               training = training
                                           )
                                       }
                                   }
                               )
)

# function for instantiating custom wrapper
layer_concrete_dropout <- function(object, 
                                   layer,
                                   weight_regularizer = 1e-6,
                                   dropout_regularizer = 1e-5,
                                   init_min = 0.1,
                                   init_max = 0.1,
                                   is_mc_dropout = TRUE,
                                   name = NULL,
                                   trainable = TRUE) {
    create_wrapper(ConcreteDropout, object, list(
        layer = layer,
        weight_regularizer = weight_regularizer,
        dropout_regularizer = dropout_regularizer,
        init_min = init_min,
        init_max = init_max,
        is_mc_dropout = is_mc_dropout,
        name = name,
        trainable = trainable
    ))
}

# sample size (training data)
n_train <- 1000
# sample size (validation data)
n_val <- 1000
# prior length-scale
l <- 1e-4
# initial value for weight regularizer 
wd <- l^2/n_train
# initial value for dropout regularizer
dd <- 2/n_train

#####################
####Dropout model####
#####################

# we use one-dimensional input data here, but this isn't a necessity
input_dim <- 1
# this too could be > 1 if we wanted
output_dim <- 1
hidden_dim <- 1024

input <- layer_input(shape = input_dim)

output <- input %>% layer_concrete_dropout(
    layer = layer_dense(units = hidden_dim, activation = "relu"),
    weight_regularizer = wd,
    dropout_regularizer = dd
) %>% layer_concrete_dropout(
    layer = layer_dense(units = hidden_dim, activation = "relu"),
    weight_regularizer = wd,
    dropout_regularizer = dd
) %>% layer_concrete_dropout(
    layer = layer_dense(units = hidden_dim, activation = "relu"),
    weight_regularizer = wd,
    dropout_regularizer = dd
)

mean <- output %>% layer_concrete_dropout(
    layer = layer_dense(units = output_dim),
    weight_regularizer = wd,
    dropout_regularizer = dd
)

log_var <- output %>% layer_concrete_dropout(
    layer_dense(units = output_dim),
    weight_regularizer = wd,
    dropout_regularizer = dd
)

output <- layer_concatenate(list(mean, log_var))

model <- keras_model(input, output)

############################
####Heteroscedastic loss####
############################

heteroscedastic_loss <- function(y_true, y_pred) {
    mean <- y_pred[, 1:output_dim]
    log_var <- y_pred[, (output_dim + 1):(output_dim * 2)]
    precision <- k_exp(-log_var)
    k_sum(precision * (y_true - mean) ^ 2 + log_var, axis = 2)
}

###############
####Testing####
###############

gen_data_1d <- function(n) {
    sigma <- 1
    X <- matrix(rnorm(n))
    w <- 2
    b <- 8
    Y <- matrix(X %*% w + b + sigma * rnorm(n))
    list(X, Y)
}

c(X, Y) %<-% gen_data_1d(n_train + n_val)

c(X_train, Y_train) %<-% list(X[1:n_train], Y[1:n_train])
c(X_val, Y_val) %<-% list(X[(n_train + 1):(n_train + n_val)], 
                          Y[(n_train + 1):(n_train + n_val)])

model %>% compile(
    optimizer = "adam",
    loss = heteroscedastic_loss,
    metrics = c(custom_metric("heteroscedastic_loss", heteroscedastic_loss))
)

history <- model %>% fit(
    X_train,
    Y_train,
    epochs = 30,
    batch_size = 10
)


####Obtain uncertainty estimates via Monte Carlo sampling####
num_MC_samples <- 20

MC_samples <- array(0, dim = c(num_MC_samples, n_val, 2 * output_dim))
for (k in 1:num_MC_samples) {
    MC_samples[k, , ] <- (model %>% predict(X_val))
}

# the means are in the first output column
means <- MC_samples[, , 1:output_dim]  
# average over the MC samples
predictive_mean <- apply(means, 2, mean)

epistemic_uncertainty <- apply(means, 2, var)

logvar <- MC_samples[, , (output_dim + 1):(output_dim * 2)]
aleatoric_uncertainty <- exp(colMeans(logvar))

df <- data.frame(
    x = X_val,
    y_pred = predictive_mean,
    e_u_lower = predictive_mean - sqrt(epistemic_uncertainty),
    e_u_upper = predictive_mean + sqrt(epistemic_uncertainty),
    a_u_lower = predictive_mean - sqrt(aleatoric_uncertainty),
    a_u_upper = predictive_mean + sqrt(aleatoric_uncertainty),
    u_overall_lower = predictive_mean - 
        sqrt(epistemic_uncertainty) - 
        sqrt(aleatoric_uncertainty),
    u_overall_upper = predictive_mean + 
        sqrt(epistemic_uncertainty) + 
        sqrt(aleatoric_uncertainty)
)

ggplot(df, aes(x, y_pred)) + 
    geom_point() + 
    geom_ribbon(aes(ymin = e_u_lower, ymax = e_u_upper), alpha = 0.3)


library(readxl)
library(dplyr)
library(GGally)

df <- read_xlsx("CCPP/Folds5x2_pp.xlsx")
ggscatmat(df)

df_scaled <- scale(df)

X <- df_scaled[, 1:4]
train_samples <- sample(1:nrow(df_scaled), 0.8 * nrow(X))
X_train <- X[train_samples,]
X_val <- X[-train_samples,]

y <- df_scaled[, 5] %>% as.matrix()
y_train <- y[train_samples,]
y_val <- y[-train_samples,]

n <- nrow(X_train)
n_epochs <- 100
batch_size <- 100
output_dim <- 1
num_MC_samples <- 20
l <- 1e-4
wd <- l^2/n
dd <- 2/n

get_model <- function(input_dim, hidden_dim) {
    
    input <- layer_input(shape = input_dim)
    output <-
        input %>% layer_concrete_dropout(
            layer = layer_dense(units = hidden_dim, activation = "relu"),
            weight_regularizer = wd,
            dropout_regularizer = dd
        ) %>% layer_concrete_dropout(
            layer = layer_dense(units = hidden_dim, activation = "relu"),
            weight_regularizer = wd,
            dropout_regularizer = dd
        ) %>% layer_concrete_dropout(
            layer = layer_dense(units = hidden_dim, activation = "relu"),
            weight_regularizer = wd,
            dropout_regularizer = dd
        )
    
    mean <-
        output %>% layer_concrete_dropout(
            layer = layer_dense(units = output_dim),
            weight_regularizer = wd,
            dropout_regularizer = dd
        )
    
    log_var <-
        output %>% layer_concrete_dropout(
            layer_dense(units = output_dim),
            weight_regularizer = wd,
            dropout_regularizer = dd
        )
    
    output <- layer_concatenate(list(mean, log_var))
    
    model <- keras_model(input, output)
    
    heteroscedastic_loss <- function(y_true, y_pred) {
        mean <- y_pred[, 1:output_dim]
        log_var <- y_pred[, (output_dim + 1):(output_dim * 2)]
        precision <- k_exp(-log_var)
        k_sum(precision * (y_true - mean) ^ 2 + log_var, axis = 2)
    }
    
    model %>% compile(optimizer = "adam",
                      loss = heteroscedastic_loss,
                      metrics = c("mse"))
    model
}

model <- get_model(1, 64)
hist <- model %>% fit(
    X_train[ ,1],
    y_train,
    validation_data = list(X_val[ , 1], y_val),
    epochs = n_epochs,
    batch_size = batch_size
)

MC_samples <- array(0, dim = c(num_MC_samples, nrow(X_val), 2 * output_dim))
for (k in 1:num_MC_samples) {
    MC_samples[k, ,] <- (model %>% predict(X_val[ ,1]))
}

means <- MC_samples[, , 1:output_dim]  
predictive_mean <- apply(means, 2, mean) 
epistemic_uncertainty <- apply(means, 2, var) 
logvar <- MC_samples[, , (output_dim + 1):(output_dim * 2)]
aleatoric_uncertainty <- exp(colMeans(logvar))

preds <- data.frame(
    x1 = X_val[, 1],
    y_true = y_val,
    y_pred = predictive_mean,
    e_u_lower = predictive_mean - sqrt(epistemic_uncertainty),
    e_u_upper = predictive_mean + sqrt(epistemic_uncertainty),
    a_u_lower = predictive_mean - sqrt(aleatoric_uncertainty),
    a_u_upper = predictive_mean + sqrt(aleatoric_uncertainty),
    u_overall_lower = predictive_mean - 
        sqrt(epistemic_uncertainty) - 
        sqrt(aleatoric_uncertainty),
    u_overall_upper = predictive_mean + 
        sqrt(epistemic_uncertainty) + 
        sqrt(aleatoric_uncertainty)
)

ggplot(df, aes(x, y_pred)) + 
    geom_point() + 
    geom_ribbon(aes(ymin = e_u_lower, ymax = e_u_upper), alpha = 0.3)
