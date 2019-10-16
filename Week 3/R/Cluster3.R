library (cluster)
library (dbscan)
library (treeClust) # for cramer ()

opt.test <- read.table ("optdigits.test.txt", sep=",")

dim (opt.test) #[1] 1797   65
#
# What do some of these digits look like?
#
show.digit <- function (dat = opt.test, i) 
{
    class <- dat[i, 65]
    vec <- unlist (dat[i,-65])
    mat <- matrix (vec, 8, 8)
    image (mat[,8:1], main = paste ("Row", i, ", Class", class), col=gray((16:0)/16),
        axes=F, xlab = "", ylab="")
}
par (mar = c(0, 0, 2, 0), mfrow = c(4, 4))
for (i in sample (nrow (opt.test), 16, replace=FALSE))
    show.digit (i = i)

#
# No need to scale here. Let's try 15 clusters. Of course there
# are only ten digits, but there are multiple ways to write a 2,
# for example.
#

table (kmeans (opt.test[,-65], centers = 15, iter.max = 100, 
         nstart = 100)$cluster, opt.test[,65])
cramer (.Last.value) # about 0.90

cramer (table (pam (opt.test[,-65], 15)$clust, opt.test[,65])) # 0.86
cramer (table (clara (opt.test[,-65], 15)$clust, opt.test[,65])) # 
# Clara is much faster, but in this case produces a lower cramer's V.

# How about using a custom distance?
# tc.dist <- treeClust.dist (opt.test[,-65], d.num = 4) # ~20 seconds?
cramer (table (pam (tc.dist, 15)$clust, opt.test[,65])) # 0.80

ag <- agnes (opt.test[,-65])
cramer (table (cutree (ag, k = 15), opt.test[,65])) # .81
#
# If you pass a vector of k's to cutree(), you get a matrix of assignments.
#
#
# Let's try dbscan. First try to find a good eps cutoff. 
#
library (dbscan)
kNNdistplot (opt.test[,-65], k = 65)
#
# Maybe...28?
#
dbscan (opt.test[,-65], 28, 65)

table (dbscan (opt.test[,-65], 28, 65)$clust, opt.test[,65])
   
      0   1   2   3   4   5   6   7   8   9
  0   0  83  63  22  65 159   3  49 120  49
  1 178   0   0 160   0  22   2   0   9 131
  2   0   1   0   0   0   1 176   0   0   0
  3   0   0   0   1   1   0   0 130   0   0
  4   0   0   0   0 115   0   0   0   0   0
  5   0   0 114   0   0   0   0   0   0   0
  6   0  98   0   0   0   0   0   0  45   0
#
# Well, the 0's are all over the place, but the rest are doing great.
# Notice that the algorithm chose only 6 clusters.
#
cramer (table (dbscan (opt.test[,-65], 28, 65)$clust, opt.test[,65])[-1,])


