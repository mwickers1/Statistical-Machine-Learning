#
# Read data in
#
super <- read.csv ("Data/super.csv")    # a data.frame
super.class <- super$critical_temp # a vector
super <- super[,names (super) != "critical_temp"] # Remove response column
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
#
# Now scale using only the training data. 
#
super.train <- scale (super.train)
# super.test <- scale (super.test, 
#                      center = attributes(super.train)$'scaled:center',
                     scale = attributes(super.train)$'scaled:scale')


library (keras)

reg <- regularizer_l1_l2 (l1 = 0.01, l2 = 0.01)

mod <- keras_model_sequential ()%>% # I give you this for free!
  layer_dense(units = 50, activation = 'sigmoid', kernel_regularizer = reg, input_shape = 81)%>%
  layer_dense(units = 100, activation = 'sigmoid', kernel_regularizer = reg )%>%
  layer_dense(units = 20, activation = 'sigmoid', kernel_regularizer = reg)%>%
  layer_dense(units = 1, activation = "softmax")




model <- keras_model_sequential() %>%
  layer_dense(units=64, activation='sigmoid', kernel_initializer='random_normal', kernel_regularizer = reg, input_shape = 81) %>%
  layer_dense(units=128, activation='sigmoid', kernel_initializer='random_normal', kernel_regularizer = reg,) %>%
  layer_dense(units=32, activation='sigmoid', kernel_initializer='random_normal', kernel_regularizer = reg,)%>%
  layer_dense(units=64, activation='sigmoid', kernel_initializer='random_normal', kernel_regularizer = reg,)%>%
  layer_dense(units=1 , activation='softmax', kernel_initializer='random_normal', kernel_regularizer=reg)


compile(mod, loss = 'mse', optimizer = optimizer_sgd(lr = 0.01, momentum = 0.5, decay = 0.01), metrics = 'accuracy')

compile(model, loss = 'mse',  optimizer = optimizer_sgd(lr = 0.01, momentum = 0.5, decay = 0.01), metrics = 'accuracy')


history <- fit(mod,super.train, super.train.y, epochs=400, batch_size=18000)

history <- fit(model,super.train,super.train.y, validation_split=0.20, epochs=400, batch_size=18000)



plot(history)

evaluate(mod,super.test, super.test.y)
ModPred <- predict(mod, super.test)


ModelPred <- predict(model,super.test)
rsquared <- cor(super.test.y, ModelPred)

sqrt(mean((super.test.y - ModelPred)^2))




# Next steps: layer_dense() one or more times, compile(), fit().

