#
# Keras bigger example. Read in the optical digits training data. 
#
library (keras)
opt.train <- read.csv ("optdigits.train.txt", header=F) #
# Column 65 is the class. Save it as a factor, and also as a matrix of one-hot vectors
opt.class.vec <- opt.train$V65               # For use in table() etc.
opt.class <- to_categorical (opt.train$V65)  # For use in keras
opt.train <- as.matrix (opt.train[,-65]) # Remove class indicator, save as matrix

opt1 <- keras_model_sequential ()
layer_dense (opt1, units = 128, activation = "relu", input_shape = 64)
layer_dense (opt1, units = 10, activation = "softmax") # 9,610 weights
#
# Default learning rate is 0.01, momentum = 0
#
compile (opt1, loss = "categorical_crossentropy",
                   optimizer = "sgd",             # default has a learning rate of 0.01, no momentum
                   metrics =  "accuracy" )
#
# Validation set. Let's use, I don't know, 823, leaving 3000 in the "regular" training set.
# And let's try 25 epochs
#
val <- sample (nrow (opt.train), 823, replace=F)
history <- fit(opt1, 
               opt.train[-val,],
               opt.class[-val,], 
               epochs = 25,
               batch_size = 3000,
               validation_data = list(opt.train[val,], opt.class[val,])
               )
plot(history)# we can see both the validation and training history
# Let's keep going!
history <- fit(opt1, 
               opt.train[-val,],
               opt.class[-val,], 
               epochs = 50,
               batch_size = 3000,
               validation_data = list(opt.train[val,], opt.class[val,])
               )
# Training set error is about 7%
tbl <- table (opt.class.vec, predict_classes (opt1, opt.train))
1 - sum (diag (tbl)) / nrow (opt.train)

# For test set error, let's re-aquire the test set and make it a matrix
opt.test <- read.csv ("optdigits.test.txt", header=F) #
opt.test.class.vec <- opt.test$V65               # 
opt.test <- as.matrix (opt.test[,-65]) # Remove class indicator, save as matrix

# Test set error is about 8%
tbl <- table (opt.test.class.vec, predict_classes (opt1, opt.test))
1 - sum (diag (tbl)) / nrow (opt.test) # maybe 10%
#
######################
##### More options
######################
opt1 <- keras_model_sequential ()
layer_dense (opt1, units = 128, activation = "relu", input_shape = 64)
layer_dense (opt1, units = 10, activation = "softmax") # 9,610 weights
#
# To change the learning rate, momentum, and/or weight decay,
# we have to specify the optimizer more explicitly.
#
compile (opt1, loss = "categorical_crossentropy",
                   optimizer = optimizer_sgd (lr = 0.02, momentum = 0.4, decay = 0.03),
                   metrics =  "accuracy" )
history <- fit(opt1, 
               opt.train[-val,],
               opt.class[-val,], 
               epochs = 50,
               batch_size = 3000,
               validation_data = list(opt.train[val,], opt.class[val,])
               )
tbl <- table (opt.test.class.vec, predict_classes (opt1, opt.test))
1 - sum (diag (tbl)) / nrow (opt.test) # maybe 10%

#############################
# I need a better model. Let's fit "harder," at the risk of over-fitting. I'll
# use two hidden layers, because what the hey. But let's train in smaller epochs -- 
# maybe update every 150 records or so, instead of after all 3000.

hard <- keras_model_sequential () # initialize
        layer_dense (hard, units = 128, activation = "relu", input_shape = 64)  # hidden 1
        layer_dense (hard, units = 32, activation = "relu")                     # hidden  2
        layer_dense (hard, units = 10, activation = "softmax")              # output layer
#
compile(hard,  loss = "categorical_crossentropy", 
                    optimizer = optimizer_sgd (lr = 0.02, momentum = 0.4, decay = 0.03),
                    metrics = "accuracy")
history <- fit(hard, 
               opt.train[-val,],
               opt.class[-val,], 
               epochs = 50,
               batch_size = 150,
               validation_data = list(opt.train[val,], opt.class[val,])
               )
# Test set error is about 6%
tbl <- table (opt.test.class.vec, predict_classes (hard, opt.test))
1 - sum (diag (tbl)) / nrow (opt.test) # 
#
# Can we get better if we keep going? Answer: a little...
history <- fit(hard, 
               opt.train[-val,],
               opt.class[-val,], 
               epochs = 50,
               batch_size = 150,
               validation_data = list(opt.train[val,], opt.class[val,])
               )
# Test set error is even smaller
tbl <- table (opt.test.class.vec, predict_classes (hard, opt.test))
1 - sum (diag (tbl)) / nrow (opt.test) # a little better...
#
# Let's use the same model, but with dropout
#
harddrop <- keras_model_sequential () # initialize
        layer_dense (harddrop, units = 128, activation = "relu", input_shape = 64)  # hidden 1
        layer_dropout (harddrop, 0.2) # applies to most recent layer
        layer_dense (harddrop, units = 32, activation = "relu")                     # hidden  2
        layer_dropout (harddrop, 0.5)
        layer_dense (harddrop, units = 10, activation = "softmax")              # output layer
#
compile(harddrop,  loss = "categorical_crossentropy", 
                    optimizer = "sgd",
                    metrics = "accuracy")
history <- fit(harddrop, 
               opt.train[-val,],
               opt.class[-val,], 
               epochs = 50,
               batch_size = 150,
               validation_data = list(opt.train[val,], opt.class[val,])
               )
# Test set error ...
tbl <- table (opt.test.class.vec, predict_classes (harddrop, opt.test))
1 - sum (diag (tbl)) / nrow (opt.test) # a little worse (!)
#
# Now add batch normalization!
#
#
harddrop <- keras_model_sequential () # initialize
        layer_dense (harddrop, units = 128, activation = "relu", input_shape = 64)  # hidden 1
        layer_batch_nomalization ()   # take the defaults
        layer_dropout (harddrop, 0.2) # applies to most recent layer
        layer_dense (harddrop, units = 32, activation = "relu")                     # hidden  2
        layer_batch_nomalization ()   # take the defaults
        layer_dropout (harddrop, 0.5)
        layer_dense (harddrop, units = 10, activation = "softmax")              # output layer
#
compile(harddrop,  loss = "categorical_crossentropy", 
                    optimizer = "sgd",
                    metrics = "accuracy")
history <- fit(harddrop, 
               opt.train[-val,],
               opt.class[-val,], 
               epochs = 50,
               batch_size = 150,
               validation_data = list(opt.train[val,], opt.class[val,])
               )
# Test set error ...
tbl <- table (opt.test.class.vec, predict_classes (harddrop, opt.test))
1 - sum (diag (tbl)) / nrow (opt.test) # a little worse (!)
