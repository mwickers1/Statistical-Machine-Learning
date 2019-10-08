#
# Principal components example 2. The iris data.
#

dim (iris) # 150 x 5, but the last column is categorical.
#
# Here are the column variances.
#
sapply (iris[,1:4], var)
# Sepal.Length  Sepal.Width Petal.Length  Petal.Width 
#    0.6856935    0.1899794    3.1162779    0.5810063 
# They add up to...

sum (sapply (iris[,1:4], var))
# [1] 4.572957

#
# They add up to 4.57. Remember that number.
# Now let's call prcomp().
#

prcomp (iris[,1:4])
# Standard deviations (1, .., p=4):
# [1] 2.0562689 0.4926162 0.2796596 0.1543862
# : : :
# : : :
#
# If you take the sd's of the new columns, and square them, and add them
# up, you get that same number 
#
sum (prcomp (iris[,1:4])$sd^2) 
# [1] 4.572957
# Note: if you use princomp(), you have to correct for the fact
# that princomp() divides by n but var() divides by n-1.
#
#
# Save and plot. This produces the scree plot.
#
iris.pr <-  prcomp (iris[,1:4])
plot (iris.pr)

#
# What else is in there?
#
names (iris.pr)
# [1] "sdev"     "rotation" "center"   "scale"    "x"   
#
# Scale and center give the scaling (since we didn't scale, these is FALSE)
# and centering (that is, column averages).
# 
iris.pr$scale
# [1] FALSE
iris.pr$center
# Sepal.Length  Sepal.Width Petal.Length  Petal.Width 
#     5.843333     3.057333     3.758000     1.199333 
#
# The "new" data is in the x object. Like the old data, it's 150 x 4.
#
dim (iris.pr$x)
# [1] 150   4
iris.pr$x[1:3,]
#            PC1        PC2         PC3         PC4
# [1,] -2.684126 -0.3193972 -0.02791483 0.002262437
# [2,] -2.714142  0.1770012 -0.21046427 0.099026550
# [3,] -2.888991  0.1449494  0.01790026 0.019968390

#
# The loadings ("rotation") are the coefficients of the linear combinations.
# princomp () has a slightly nicer display here.

iris.pr$rot
#                      PC1         PC2         PC3        PC4
# Sepal.Length  0.36138659 -0.65658877  0.58202985  0.3154872
# Sepal.Width  -0.08452251 -0.73016143 -0.59791083 -0.3197231
# Petal.Length  0.85667061  0.17337266 -0.07623608 -0.4798390
# Petal.Width   0.35828920  0.07548102 -0.54583143  0.7536574

#
# Neat pictures.
# 
#install.packages ("rgl")
library (rgl)
cc <- rep (c("red", "blue", "black"), each = 50)
plot3d (iris[,1:3], col=cc) # original data; it's at an angle

# Rotated data, mostly parallel to the new coordinate axes
plot3d (iris.pr$x[,1:3], col=cc)

