#
# Showing that a neural network is a generalization of 
# linear and logistic regression. Example 1: linear regression
# on the TACNAV data
#
tacnav <- read.csv ("tacnav.csv")
# Simple plot, 
plot (RadERROR ~ TIME, data = tacnav, 
    col = ifelse (tacnav$site == "PAX RIVER", "red", "blue"), pch=19)
# Here's a fairly cool thing: lattice, which makes a set of 
# plots but automatically controls axes, labeling, etc.
# This is a big subject, but here are some examples
library (lattice)
xyplot (RadERROR ~ TIME, data = tacnav) # not exciting yet. Quadratic?
xyplot (RadERROR ~ TIME | site, data = tacnav, layout = c(1, 2))
# Modify "panel" function to both plot points and also draw regression lines
xyplot (RadERROR ~ TIME | site, data = tacnav, layout = c(1, 2),
    panel = function (x, y) {panel.xyplot (x, y); panel.lmline (x, y)})
#
# So what do we see? Pax has a smaller slope, smaller variance, and
# also less of a TIME range -- so maybe that quadratic thing was an illusion.
#
# Let's fit some regressions.
#
mynet <- nnet (RadERROR ~ TIME + SITE, size = 0, skip = T, 
      linout = T, data = tacnav)
mynet
mynet$wts
summary (mynet)
lm (RadERROR ~ TIME + SITE, data = tacnav)
#
# Okay, let's fit a logistic regression
#
glm (Species == "versicolor" ~ ., data = iris, family = binomial)$coef
mynet <- nnet (Species == "versicolor" ~ ., data = iris, size = 0, 
                 skip = T, entropy=TRUE)
mynet$wts

#
# But here's the cool part: multinomial regression
#
mynet <- nnet (Species ~ ., data = iris, size = 0, 
                 skip = T, entropy=TRUE) # 5 -> 3, so 15 weights
mynet$wts # 15 parameters
predict (mynet, iris[48:52,])
summary (mynet)

# We can also use multinom(), which calls nnet. The results aren't identical,
# because multinom sets one Species to baseline and estimates contrasts between
# that baseline and each other species individually.
set.seed (1203)
multi <- multinom (Species~ ., data = iris, maxit = 200)
table (predict  (multi), iris$Species) # Of course this is not on a test set!

