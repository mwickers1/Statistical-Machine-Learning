#######################################################
#
#   NN3: Intro to Keras
#
#
#########################################################
#
# First, install keras with install.packages ("keras") and then
# install_keras(). You only need to do this once
#
library(keras)
help(package = "keras")       # Maybe keep open to see the keras NN options
#
########################################################
#
# Fit the first NN
#
########################################################
#
#  Shape the data
#
x <- as.matrix(iris[,1:4])                           # this must be a matrix and not a data frame
y <- to_categorical(as.numeric(iris$Species)-1, 3)   # this takes some time...
# Y is now a set of "1-hot" vectors
y[c(1:3, 51:52),]
#
# Now, let's define a neural network architecture
#
iris.keras <- keras_model_sequential() # initialize
#
# The first (and only) hidden layer will have 3 hidden nodes. 
#
layer_dense(iris.keras, units = 3, activation = "sigmoid", input_shape = 4)

iris.keras # So far we have a model with one fully connected ("dense") layer
           # with 15 parameters: (4 input + bias) * (3 hidden)
           # The "input_shape" is required and describes the input layer. 
#
# Now add the output layer. This has three units because we have three classes.
# So this adds (3 hidden + bias) * 3 output = 12 weights.

layer_dense(iris.keras, units = 3, activation = "softmax")

iris.keras   # Now we have two layers (excluding the inputs), and a total of 27 weights
#
# "Compile," e.g. tell it: the loss function, the algorithm, and performance metric,
#   Stochastic gradient descent (sgd) is the valnilla optimizer and a good place to start.
#
iris.keras$optimizer # nothing -- not set

compile (iris.keras, loss = "categorical_crossentropy", optimizer = "sgd", metrics = "accuracy")
#
# The "categorical_crossentropy" is multinomial deviance loss. Notice that we don't
# need to assign the result of compile() to a new object; it operates "in place."
#
iris.keras$optimizer # Now set to...something SGD related

# Fit the model. 
#  This fit will see the entire data set (batch_size = 150) 20 times (20 "epochs").  
#  With batch_size at 150, the weights are only updated after seeing all of the data.
#
history <- fit(iris.keras, 
               x = x,
               y = y,
               epochs = 20,
               batch_size = 150,
               shuffle = FALSE      #default is TRUE, shuffles training data at each epoch.
               )
plot(history)   # if you want to reproduce the plot, we aren't in love with the smoothers used here
(history.df <- as.data.frame( history$metrics) ) # to save the output
#
# Predict
#
c.hat <- predict_classes(iris.keras, x) # Predicted classes start at 0,  C/Python style
table(iris$Species, levels(iris$Species)[c.hat + 1]) # This is not very good
predict_proba(iris.keras,x) [1:10,]     # Predicted probabilities
#
#  Let's see the weights 
#
( ww <- get_weights(iris.keras))
#
# How about the outputs from (or inputs to) a hidden layer?
#
  
############################################################################################
#
# Now lets be a bit smarter about this
#
############################################################################################
#
# First standardize the data. Keras will not do this for you.
# x.mean and x.sd need to be saved. They will be used to pre-process any new data fed to your NN.
#          e.g. 
#          newx <- scale (newx, center = x.mean, scale = x.sd) # Just as with Prin Comps.
#
x <- scale(as.matrix(iris[, 1:4]))
attributes(x)# the mean and standard deviation are stored with x, these are model parameters
x.mean <-attributes(x)$'scaled:center'
x.sd <- attributes(x)$'scaled:scale'
#
# Now add  
#     1. add more hidden units in the first hidden layer,
#     2. add L2-Norm regularization, 
#     3. ReLu activation functions,
#     4. Try to validate ( done at fitting time).
#
iris.keras <- keras_model_sequential() # initialize

# 50 hidden units, ReLu activation, and regularization. These choices all deserve
# more consideration that I'm going to give right now.
layer_dense(iris.keras, units = 50, activation = "relu", input_shape = 4,
               kernel_regularizer = regularizer_l2(0.001) )        

# Connecting hidden layer to output layer, which again has three nodes
layer_dense(iris.keras, units = 3, activation = "softmax",
                kernel_regularizer = regularizer_l2(0.001) )          # notice that regularization needs to be added to include each layer's weights
compile (iris.keras,  loss = "categorical_crossentropy", optimizer = "sgd", metrics = "accuracy")
#
# And try to validate with 20% of the training set
# The argument validation_split = 0.20 will select a 20% validation set.
# BUT DON'T USE THIS!! It is not chosen randomly.
# To be safe, create your own validation set, perhaps as follows 
# 
set.seed(1234)                        # set.seed() will not set the TensorFlow seed
val.rows <- sort (sample(150,30))     # Random sample of size 30 for validation
#
history <- fit(iris.keras, 
               x = x[-val.rows ,  ],  # omit the 30 for training
               y = y[-val.rows ,  ],
               validation_data = list(x[val.rows, ], y[val.rows, ] ), # clunky, but this works
               epochs = 300,                 # increase the number of gradient descent updates
               batch_size = 120              # update after seeing all 120 obs in the training set
               )
plot(history) # I am getting 93% training accuracy, which is not great, we have some work to do
#
# Encouraging. If we fun the "fit" command again on iris.keras, it resumes from where
# we left off. When I did this, the validation set's performance was actually better
# than the test set's. I'm not sure what to make of that.
#     
history <- fit(iris.keras, 
               x = x[-val.rows ,  ],  # omit the 30 for training
               y = y[-val.rows ,  ],
               validation_data = list(x[val.rows, ], y[val.rows, ] ), # clunky, but this works
               epochs = 300,                 # increase the number of gradient descent updates
               batch_size = 120              # update after seeing all 120 obs in the training set
               )
 
c.hat <- predict_classes(iris.keras, x) # Predicted classes start at 0,  C/Python style
table(iris$Species, levels(iris$Species)[c.hat + 1]) # Better





