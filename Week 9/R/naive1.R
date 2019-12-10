#
# Splice data via Naive Bayes
#
splice.orig <- read.fwf ("//comfort/ORProjects$/Clustering/Data/Splice/Splice.data.txt", widths=c(2, 5, 32, rep (1, 60)))
splice <- splice.orig
splice <- splice[,-(1:3)] # delete three columns
# Remove 16 rows with D, N, S, or R in them.
splice <- splice[!apply (splice, 1, function (x) any (is.element (x, c("D", "N", "S", "R")))),]
# Class distribution
splice.class <- splice.orig[match (row.names (splice), row.names (splice.orig)),1]
#
# Added 22 Jun 2015: get rid of "empty" factor levels
#
for (i in names (splice)) splice[[i]] <- factor (as.character(splice[[i]]))
splice$class <- splice.class

set.seed (1494) # Birth year of Suleiman the Magnificent
splitest <- sample (nrow (splice), 350, replace=FALSE)

(splinb <- naiveBayes (class ~ ., data = splice, subset = -splitest))
table (splice[splitest, "class"], predict (splinb, splice[splitest,]))

#
# Biopsy data via naive Bayes
#
library (MASS)
bio <- biopsy[complete.cases (biopsy), -1]

sapply (bio, table)

bio$V1 <- cut (bio$V1, c(1, 3, 5, 10, 99), right = F, include.lowest = TRUE)
bio$V2 <- cut (bio$V2, c(1, 2, 10, 99), right = F, include.lowest = TRUE)
bio$V3 <- cut (bio$V3, c(1, 2, 10, 99), right = F, include.lowest = TRUE)
bio$V4 <- cut (bio$V4, c(1, 2, 10, 99), right = F, include.lowest = TRUE)
bio$V5 <- cut (bio$V5, c(1, 3, 8, 99), right = F, include.lowest = TRUE)
bio$V6 <- cut (bio$V6, c(1, 2, 10, 99), right = F, include.lowest = TRUE)
bio$V7 <- cut (bio$V7, c(1, 2, 3, 4, 99), right = F, include.lowest = TRUE)
bio$V8 <- cut (bio$V8, c(1, 2, 10, 99), right = F, include.lowest = TRUE)
bio$V9 <- cut (bio$V9, c(1, 2, 99), right = F, include.lowest = TRUE)

sapply (bio, table)

set.seed (1494) # Birth year of Suleiman the Magnificent
biotest <- sample (nrow (bio), 135, replace=FALSE)

library (e1071)
(bionb <- naiveBayes (class ~ ., data = bio, subset = -biotest))
#
# Let's compare to a logistic regression
#
(glm.1 <- glm (class ~ ., data = bio, family = "binomial", subset = -biotest))
#
# What's happening here?
#
table (bio$V1, bio$class) # egad

library (pROC)
 plot (roc (bio[biotest, "class"], predict (bionb, bio[biotest,], type = "raw")[,2])) 
lines (roc (bio[biotest, "class"], predict (glm.1, bio[biotest,], 
           type = "response")), col = "red") # a toss-up
#
# Slightly different styles...
#
plot ( predict (bionb, bio[biotest,], type = "raw")[,2], 
       predict (glm.1, bio[biotest,], type = "response"))


