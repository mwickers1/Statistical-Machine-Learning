########################################
#
#  Week 4: "Real" NN using iris data
#
########################################
library(nnet)
#
# Look at the iris data
#
str(iris)
#
# Play with colors a bit and make a plot
#
col <- c("orange","blue","red") 
col <- adjustcolor(col,alpha=.4) # cool function
col <- col[iris$Species]
pairs( iris[ , -5], col = col, pch =19, cex=1.2)
#
# Look at the nnet help page
#
help(nnet)
#
# Now train a NN 
#    nnet "only" supports one hidden layer
#    Species is a factor, so our nnet should give back 
#    a vector of predicted Species probabilities for each obs.
#
set.seed(94) # set random number seed, for reproducibility
# The net initializes the weights randomly, then tries to optimize,
# but beware of local minima

iris.94 <- nnet(Species ~ ., data = iris, size = 3)
iris.94
#
# How well did we do?
#
table(iris$Species, predict(iris.94, type = "class"))
summary(iris.94) # note the spread of the weights
#
# Let's try to start where iris.94 left out and "jitter" our way out of the flat spot
#
iris.x <- nnet(Species~., data=iris, size=3, 
       Wts=jitter(iris.94$wts))# Nope!
#
# Try different starting values
#
set.seed(80)
iris.80 <- nnet(Species ~ ., data = iris, size = 3)
table (iris$Species, predict (iris.80,type = "class")) # much better
#
# Standardize the variables and regularize (with "decay" parameter)
# If we scale, we need to save the mean and SD values for use with
# a test set.
#
set.seed(94)
iris.scaled <- scale (iris[,1:4], center = T, scale = TRUE)
iris.new <- data.frame(iris.scaled, Species=iris[,5])
iris.W.94 <- nnet(Species ~., decay = .01, data = iris.new, size = 3)
summary (iris.W.94) # less spread-out weights
attributes (iris.scaled) # keep track of mean and SD
# We would scale new data like this:
# newdata <- scale (newdata, 
#    mean  = attributes (iris.scaled)$'scaled:center',
#    scale = attributes (iris.scaled)$'scaled:scale')
#
# Let's try something a little bigger. Let's use the test set
# from optdigits to train, and the training set as a test set
# (for reasons of size and time).
#
opt.test <- read.csv ("optdigits.test.txt", header=F) # remember this?
opt.test$V65 <- factor (opt.test$V65)
set.seed (1230)
opt.nnet <- nnet (V65 ~ ., data = opt.test, size = 10, maxit = 400, 
                  decay = 0.01)
# Let's stop here. How did we do?
(train.tbl <- table (opt.test$V65, 
               predict (opt.nnet, type = "class"))) # training set
1 - sum (diag(train.tbl)) / sum (train.tbl) # 1%-ish error rate

# How do we do on new data?
opt.train <- read.csv ("optdigits.train.txt", header=F) # 
opt.train$V65 <- factor (opt.train$V65)
(test.tbl <- table (opt.train$V65, 
               predict (opt.nnet, opt.train, type = "class"))) # test set
1 - sum (diag(test.tbl)) / sum (test.tbl) # 10%-ish error rate

#
# Can we do better?
#
opt.nnet <- nnet (V65 ~ ., data = opt.test, size = 10, maxit = 400, 
                  Wts = opt.nnet$wts, decay = 0.01) # keep fitting
(train.tbl <- table (opt.test$V65, 
               predict (opt.nnet, type = "class"))) # training set
1 - sum (diag(train.tbl)) / sum (train.tbl) # 0% error rate (?!)

(test.tbl <- table (opt.train$V65, 
               predict (opt.nnet, opt.train, type = "class"))) # test set
1 - sum (diag(test.tbl)) / sum (test.tbl) # 8% error rate


