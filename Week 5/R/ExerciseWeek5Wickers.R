library(keras)

data <- read.csv('Data/super.csv', stringsAsFactors = F)

fitMod <- lm(critical_temp ~ ., data = data)
summary(fitMod)


super.class <- data$critical_temp # a vector
super <- data[,names(data) != "critical_temp"] # Remove response column
#
# Test/train split
#
set.seed (6881)
train.ind <- sample (nrow (super), 18000, replace=FALSE)
super.train <- as.matrix (super[train.ind,]) # no categoricals
super.train.y <- matrix (super.class[train.ind], ncol = 1) # as matrix
# If there had been factors we would have had to turn them into one-hot sets.

super.test <- as.matrix (super[-train.ind,]) 
super.test.y <- matrix (super.class[-train.ind], ncol = 1)

#Now scale using only the training data.
#
super.train <- scale(super.train)
super.test <- scale(super.test,
                    center = attributes(super.train)$'scaled:center',
                    scale = attributes(super.train)$'scaled:scale')



reg <- regularizer_l1_l2 (l1 = 0.01, l2 = 0.01)
mod <- keras_model_sequential ()%>% # I give you this for free!
  layer_dense(units = 81, activation = 'sigmoid', kernel_regularizer = reg, input_shape = 81)%>%
  layer_dense(units = 5, activation = 'sigmoid', kernel_regularizer = reg)%>%
  # layer_dense(units = 5, activation = 'sigmoid', kernel_regularizer = reg)%>%
  layer_dense(units = 1)



compile(mod, loss = 'mse', optimizer = optimizer_sgd(lr = 0.01, momentum = 0.5, decay = 0.01), metrics = 'mse')


val.ind <- sample (18000, 2000, replace=F)

history <- fit(mod,x = super.train[-val.ind,], y = super.train.y[-val.ind,],
               validation_data = list (super.train[val.ind,], super.train.y[val.ind,]),
               epochs=400, batch_size=18000)

plot(history)

ModelPred <- predict(mod,super.test)


cor(super.test.y, ModelPred)^2

sqrt(mean((super.test.y - ModelPred)^2))
