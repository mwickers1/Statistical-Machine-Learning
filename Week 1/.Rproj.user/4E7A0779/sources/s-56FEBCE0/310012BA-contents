#
# Very simple PCA example. We start with a 7x2 data set of test scores.
#
students <- data.frame (
T1 = c(24, 28, 33, 37, 42, 44, 47),
T2 = c(21, 31, 30, 40, 45, 47, 44))

plot (students[,1], students[,2], type = "n", xlim = c(20, 55), ylim = c(20, 55))
text (students[,1], students[,2], 1:7)
abline (lm (students[,2] ~ students[,1])) # regression line
#
# Now principal compoments. I will use the negative of the 
# second column, just because it makes the picture nicer.
#
win.graph ()
prcomp (students[,1:2])           # Check out the result
(p1 <- prcomp (students[,1:2])$x)  # the scores, i.e. new data
plot (p1[,1], -p1[,2], type = "n", xlim = c(-20, 15), ylim = c(-20, 15))
text (p1[,1], -p1[,2], 1:7)
abline (lm (p1[,2] ~ p1[,1])) # horizontal line at y = 0

dist (students)
dist (p1) # inter-point distances are unchanged!
#
# Let's add a few more columns
#
students <- data.frame (
T1 = c(24, 28, 33, 37, 42, 44, 47),
T2 = c(21, 31, 30, 40, 45, 47, 44),
T3 = c(23, 30, 34, 36, 43, 48, 46))
students$Sum <- with (students, T1 + T2 + T3)
students$Mean <- rowMeans(students[,1:3])
#
# This is 5-dimensional data -- but it's "approximately" one-dimensional
# and exactly three-d.
#
plot (prcomp (students)) # scree plot
round (prcomp (students)$sd, 5)
#
# Proportion of variance "explained," by component
#
round (prcomp (students)$sd^2 / sum (prcomp (students)$sd^2), 5)
#
# and the cumulative version...
#
cumsum (round (prcomp (students)$sd^2 / sum (prcomp (students)$sd^2), 5))


