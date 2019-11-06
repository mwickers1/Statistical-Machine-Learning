###############################################################
#
# This script shows you how to:
#
#     Get the MNIST data, display it, and re-shape it to use in keras
#     Train a sequence of NN that use the ideas discussed in class so far 
#     Illustrates some of the keras features
#
# It has some bonus content at the end.
# 
###############################################################
library(keras) 
help( package = "keras" )                           # if you want to see a list of commands and options
#
# Extract the data; the MNIST data comes with keras as a list
#
mnist <- dataset_mnist()
train_images <- mnist$train$x     # 60000 images ( there is a test set of an additional 10,000 images)
train_labels <- mnist$train$y     # 60000 labels
#
# The trainig set is a 3D array ( observations x width x height )
#
str(train_images)
#
# Look at a few images. Pixel values are grey tones between 0 (white) and 255 (black)
#
index <- sample(60000,9)                     # sample 9 
par(mfrow = c(3,3),mar = c(0,0,0,0) )        # show nine plots in 3 rows and 3 columns
for ( i in 1:9){ 
  plot(as.raster(mnist$train$x[index[i],,]/255))
  text(28/2, 28/2, train_labels[index[i]], cex = 5, col = "red")
}
par(mfrow = c(1,1), mar = c(5,4,4,2) + 0.5)  # reset the plot window
#
#  Reshape the inputs into a 2D matrix with 60,000 rows and 784 = 28x28 columns one per pixel
#  And standardize them so that all inputs are between 0 and 1
#
train_images <- array_reshape(train_images, c(60000, 28 * 28))              # Use keras function for row-wise reshaping, and not column-wise
str(train_images)                                                           # a 2D matrix now
train_images <- train_images/255                                            # rescale values to be between 0 and 1

plot(as.raster(matrix(train_images[10,],byrow = T, ncol = 28, nrow = 28)))  # A quick check to see if we did this the right way
#
# The output is categorical .... 
#   TensorFlow is Python, so we need to use their conventions
#   If this package was really good, we wouldn't need to do this.
#
table(train_labels)                            # the target levels are coded starting from 0 ala Python
train_labels <- to_categorical(train_labels) 
str(train_labels)                              # lookie here it made 60,000 "one-hot" vectors each of length 10.
#
# Select 50,000 rows to use for training and the remaining 10,0000 for validation. 
#
set.seed(101010)
index.train <- sort(sample(60000,50000)) # just use 50,000 to train and 10,000 to validate
#########################################################################################
#
# Network 1: one layer 512 units, 
#            Use gradient descent update i.e. update only after each epoch, 
#            no regularization, default learning rate, no momentum
#            we select training and validation set
#
#########################################################################################
network1 <- keras_model_sequential()                                            # initialize
layer_dense(network1, units = 512, activation = "relu", input_shape = 28 * 28)  # first hidden layer
layer_dense(network1, units = 10, activation = "softmax")                       # output layer
network1                                                                        # 407,050 weights to train
#
# Compile, e.g. tell it: the loss function, the algorithm, and performance metric
#
compile( network1,  loss = "categorical_crossentropy",
                   optimizer = "sgd",                                          # default has a learning rate of 0.01, no momentum
                   metrics =  "accuracy" )
#
# Train a network and save the history. 
#
history <- fit(network1, 
               train_images[index.train,],
               train_labels[index.train,],
               epochs = 5,
               batch_size = 50000,
               validation_data = list( train_images[ -index.train, ], train_labels[ - index.train, ] )
               )
plot(history)                                                                 # we can see both the validation and training history
#
# Let's train for 10 more epochs 
#   Here we start with the weights from the previous step
#
history.more <- fit(network1, 
               train_images[index.train,],
               train_labels[index.train,],
               epochs = 20,
               batch_size = 50000,
               validation_data = list( train_images[ -index.train, ], train_labels[ - index.train, ] )
               )                                                             # better, I get about a 30% validation rate
#
# Optional: Some R code to combine the histories and extract what you need for plotting
#
str(history)                                                 # its a list and metrics is a list too
history.list <-list(history, history.more)            # make a list of history lists, you might have updated more than once
# a function to do the extracting
extract.history <- function(hist.list){
                      history.all<-lapply( hist.list, function(thing){
                          do.call("cbind",thing$metrics)      # make a matrix of the four metrics
                       })                                     # history.all this is a list of matrices
                      history.all <- data.frame(do.call("rbind", history.all)) # combine the rows to make one df
                    }
history.all <- extract.history( history.list) # a data frame with all histories
#
# Plot ... not elegant, but it works
#
my.plot <- function(history.all){ 
       plot(1:25, (1:25)/25,type = "n", xlab = "epoch", ylab = "Accuracy")
       lim <- par("usr")
       rect(lim[1]-1, lim[3]-1, lim[2]+1, lim[4]+1, col = "light grey", border = "light grey")
       abline( v = seq(0,50,by = 5), h = seq(0, 1.0, by = 0.1), col = "white", lwd = 2)
       points(history.all$val_acc, bg = "turquoise", pch = 21, type = "b")
       points(history.all$acc, bg = "salmon1", pch = 21, type = "b")
       box()
       legend("topright", inset = 0.1,legend = c("Training","Validation"),
               lty = 1, lwd = 2, col = c("salmon1", "turquoise"), bg = "white")
               }
my.plot(history.all)

##########################################################################
#
#
#   Network number 2: Let's update after each batch of 128 this is a true SGD
#                     (why 128 and not 100?, for GPUs, batch sizes that are powers of two seem to work better)
#
#
#############################################################################
#
# Re-initialize and compile model to start from scratch. Otherwise fitting will start where it left off
#
network2 <- keras_model_sequential() # initialize
         layer_dense(network2, units = 512, activation = "relu", input_shape = 28 * 28)
         layer_dense(network2, units = 10, activation = "softmax")
compile( network2,  loss = "categorical_crossentropy", optimizer = "sgd", metrics = "accuracy" )
#
# Optional: write a "callback" function to keep track of updates between epochs (This is an "R6Class" object)
#
LossHistory <- R6::R6Class("LossHistory",
                           inherit = KerasCallback,
                           public = list(losses = NULL,
                                         on_batch_end = function(batch, logs = list()){
                                                          self$losses <- c(self$losses, logs[["loss"]])
                                                                                     }
                ))
hist2 <- LossHistory$new()                  #starts hist2 from scratch, with out this, the callback would add to hist2
history2 <- fit(network2, 
               train_images[index.train,],
               train_labels[index.train,],
               epochs = 10,                 # Train only a few epochs
               batch_size = 128,
               validation_data = list( train_images[ -index.train, ], train_labels[ - index.train, ] ),
               callbacks = list(hist2)     # optional, to update hist2 will be updated
               )
plot(history2)                             # already much better, training and val error are about 92% 
plot(hist2$losses, type = "l")             # lots of variablity and learning rate looks to be a bit to high
#######################################################################
#
#  Network 3: The previous network is not overfit even with no regularization
#             Try to overfit
#           
######################################################################
network3 <- keras_model_sequential() # initialize
         layer_dense(network3, units = 512, activation = "relu", input_shape = 28 * 28)  # hidden layer 1
         layer_dense(network3, units = 100, activation = "relu")                         # hidden layer 2
         layer_dense(network3, units = 10, activation = "softmax")                       # output layer
network3    # 454,230 weights
compile( network3,  loss = "categorical_crossentropy", 
                    optimizer = "sgd",
                    metrics = "accuracy" )
history3 <- fit(network3, 
               train_images[index.train,],
               train_labels[index.train,],
               epochs = 10,
               batch_size = 128,
               validation_data = list( train_images[ -index.train, ], train_labels[ - index.train, ] )
                )
plot(history3)     # 93.47% accuracy, but  not over-fitting
############################################################################
#
# Network 4
#     We could train a deeper NN and add more units to each layer to try to over fit, but... 
#     The learning rate is too high; let's reduce the learning, by half. 
#
###############################################################################
network4 <- keras_model_sequential() # initialize
         layer_dense(network4, units = 512, activation = "relu", input_shape = 28 * 28)
         layer_dense(network4, units = 100, activation = "relu")
         layer_dense(network4, units = 10, activation = "softmax")
compile( network4,  loss = "categorical_crossentropy", 
                   optimizer = optimizer_sgd ( lr = 0.005,    # default lr = 0.01
                                               momentum = 0, # default = 0, here is where we add momentum
                                               decay = 0),   # default = 0, we can decrease the lr at epoch   
                    metrics = "accuracy" )
history4 <- fit(network4, 
               train_images[index.train,],
               train_labels[index.train,],
               epochs = 10, 
               batch_size = 128,
               validation_data = list( train_images[ -index.train, ], train_labels[ - index.train, ] )
                            )
#
# Compare the training loss for lr = 0.005 and lr = 0.01
#
plot(history4$metrics$loss, col = "red", type = "b", ylim = c(0,1.6))
lines(history3$metrics$loss, col = "blue", type = "b")
legend("topright", inset = 0.02,legend = c("lr = 0.005", "lr = 0.01"), col = c("red","blue"), lwd = 2)
################################################################################
#
#   Network 5: There are several ways to decrease the learning rate with training.
#      1. In the compile function, the optimizer_sgd() function has an arguement decay,
#         i.e. compile(... optimizer = optimizer_sgd( lr = 0.1, decay = .999), ...), but it looks
#         like the learning rate is decreased at every update... that seems like too much
#      2. The other way is to use a built-in call back function like call back function like
#         callback_reduce_lr_on_plateau()
#      3. Use an optimizer like rmsprop, adadelta, etc. that schedule learning rate and momentum ...
#    
##############################################################################
network5 <- keras_model_sequential() # initialize
         layer_dense(network5, units = 512, activation = "relu", input_shape = 28 * 28)
         layer_dense(network5, units = 100, activation = "relu")
         layer_dense(network5, units = 10, activation = "softmax")
compile( network5,  loss = "categorical_crossentropy", 
                    optimizer = "rmsprop",                       # adaptive learning rate, for these the default parameters ofter work well   
                    metrics = "accuracy" )

history5 <- fit(network5, 
               train_images[index.train,],
               train_labels[index.train,],
               epochs = 10,
               batch_size = 128,
               validation_data = list( train_images[ -index.train, ], train_labels[ - index.train, ] )
                )                                                    # 97.7% accuracy looks like we need some regualrization here too
##############################################################################
#
# Network 6 
# What now? Now we go back to the slides and talk about dropout and batch normalizaiton
# The rest of the R code shows you how use these in keras
#      1. Try a really big network, and lots of epochs.
#      2. Regularize with L2 penalty and use dropout
#      3. Skip momentum, for no particular reason.
#      4. And in batch normalization to speed things up.
#
###############################################################################
#
# Regularization and dropout and batch normalization occur layer by layer. 
#
network6 <- keras_model_sequential() # initialize
# Layer 1
layer_dense(network6, units = 800, activation = "relu", input_shape = 28 * 28,
               kernel_regularizer = regularizer_l2(0.001))    # add regularizer
layer_dropout(network6, rate = 0.5)                           # 50% dropout for the units going into this layer; seed may be specified here
layer_batch_normalization(network6)
# Layer 2
layer_dense(network6, units = 800, activation = "relu",
             kernel_regularizer = regularizer_l2(0.001))  
layer_dropout(network6, rate = 0.5)
layer_batch_normalization(network6)
# Output Layer
layer_dense(network6, units = 10, activation = "softmax",
                kernel_regularizer = regularizer_l2(0.001))

compile( network6,  optimizer = "rmsprop",loss = "categorical_crossentropy", metrics = c("accuracy") )
history6 <- fit(network6, 
               train_images[index.train,],
               train_labels[index.train,],
               epochs = 10,
               batch_size = 128,
               validation_data = list( train_images[ -index.train, ], train_labels[ - index.train, ] )
                             )
plot(history6)                                                 # Well, heck .... a validation error of 96.2% in 10 epochs. Try 100. 



#######################################################################################################################
#######################################################################################################################
#
#
#  Something different: 
#  Transfer Learning
#    Build a network where the first layers are from the VGG16 network. 
#    https://cran.r-project.org/web/packages/keras/vignettes/applications.html#
#
########################################################################################################################
########################################################################################################################
#
#   Let's just try out one of these fancy models and see if we can use it
#   Download the entire vgg16 model
#
model <- application_vgg16(weights = "imagenet")
model # cool 138,357,544 trained parameters!!!!! but the input is 1 by 224 by 224 by 3
#
# Find an image from ImageNet e.g. 
#               img_path <-"http://farm3.static.flickr.com/2394/2235169574_a573a0a0e1.jpg"
#
img_path <- "elephant.jpg"    # I uploaded the .jpg file into the AWS home directory 
#
# Pre-process image
#
img <- image_load(img_path, target_size = c(224,224))  # this will resize the image to 224 x 224 
img <- image_to_array(img)                             # this is a 3D array
plot(as.raster(img/255))
#
# Reshape as a 4D array with some normalizaiton
#
img <- array_reshape(img, dim = c(1,224,224,3))       # we need a 4D array
img <- imagenet_preprocess_input(img)                 # this does chanel-wise color normalization... it really isn't needed
dim(img)
#
# Throw it in the model....
#
p.hat <- predict(model, img  )              # this is a vector of 1000 probabilities, one for each class
imagenet_decode_predictions(p.hat, top = 3) # it really is an elephant
############################################################################################
#
#
#   How can we use the vgg16 to construct a model to be used for transfer learning?
#        Pretend your supervised dataset consists of low-resolution 56 x 56 color images 
#
############################################################################################
conv_base <- application_vgg16( weights = "imagenet",
                                include_top = FALSE,       # get rid of the output layer and the weights going to it
                                input_shape = c(56,56,3) ) # change the input dimensions to color 56 x 56 images 

conv_base                 # 14,714,688 trainable parameters (fewer than before because the input image is smaller)
#
# One method: freeze the weights. We would freeze all or some of the weights if we use conv_base in a keras model directly
#
freeze_weights(conv_base) # now there are 0 trainable weights
conv_base
#
# Now use this conv_base in your new NN
#
model.2 <- keras_model_sequential()%>% conv_base 
layer_flatten(model.2)  
layer_dense(model.2, 100, activation = "relu")
layer_dense(model.2,1,activation = "sigmoid")    # is say the output is the binary variable indicating AK-47 or not

model.2


