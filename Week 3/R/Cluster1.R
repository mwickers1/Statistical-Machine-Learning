#
# Clustering examples. kmeans() is in base R, but the rest is in the
# "cluster" package.
#
# First, let's plot by PCs.
#
state.pr <- prcomp (state.x77, scale=T)

plot (state.pr$x[,1], state.pr$x[,2], type = "n")
text (state.pr$x[,1], state.pr$x[,2], state.abb)
#
#
library (cluster)
#
# k-means example. We already know to scale the columns of state.x77. Let's use 5 cluster.
#
state.km <- kmeans (scale (state.x77), 5)
#
# How big are the clusters?
#
table (state.km$cluster)

 1  2  3  4  5 
12 12 16  1  9  # Your results will probably differ!
#
# But this is only a local optimum; if we run it again, we can get different results.
#
state.km2 <- kmeans (scale (state.x77), 5)
table (state.km2$cluster)

 1  2  3  4  5 
 1  8 11  5 25 # Again, this algorithm is variable
#
# Here's how the two compare. Notice that the two-way table has lots of
# zeros in it. The more zeros, the more agreement.
#
table (state.km$cluster, state.km2$cluster)
   
     1  2  3  4  5
  1  0  7  0  5  0
  2  0  0 11  0  1
  3  0  0  0  0 16
  4  1  0  0  0  0
  5  0  1  0  0  8
#
# So in this example the first clustering produced a cluster it named "1" with 
# 12 members (top row), but the second clustering put seven of those, plus one 
# more state, into its cluster "2," and the other five into cluster "4."
#
# Let's set nstart to a large number to try to get a global optimum.
#
state.km <- kmeans (scale (state.x77), 5, nstart=1000, iter.max=100)
state.km2 <- kmeans (scale (state.x77), 5, nstart=1000, iter.max=100)
#
# These two runs agree, so maybe we're at the optimum.
#
table (state.km$cluster, state.km2$cluster)
   
     1  2  3  4  5
  1  1  0  0  0  0
  2  0  0 11  0  0
  3  0  0  0 17  0 # Yours will probably look something like this
  4  0  3  0  0  0
  5  0  0  0  0 18
#
# Of course a matrix of total agreement need not be diagonal; it can have permuted
# columns or rows. Here's what the output looks like
#
> state.km
K-means clustering with 5 clusters of sizes 1, 11, 17, 3, 18

Cluster means:
  Population     Income Illiteracy   Life Exp      Murder    HS Grad      Frost       Area
1 -0.8693980  3.0582456  0.5413980 -1.1685098  1.06242932  1.6828035  0.9145676  5.80934967
2 -0.2269956 -1.3014617  1.3915271 -1.1773136  1.09198092 -1.4157826 -0.7206500 -0.23402899
3  0.3101316  0.4583003 -0.2016973  0.0812333 -0.02909674  0.1534059 -0.3992647 -0.29398491
4  2.8948232  0.4869237  0.6507713  0.1301655  1.01728104  0.1393257 -1.1310576  0.99272004
5 -0.5883532  0.1114420 -0.7984253  0.6859720 -0.86841211  0.6036071  0.9551809 -0.06752485

Clustering vector:
       Alabama         Alaska        Arizona       Arkansas     California 
             2              1              3              2              4 
      Colorado    Connecticut       Delaware        Florida        Georgia 
             5              5              3              3              2 
...

Within cluster sum of squares by cluster:
[1]  0.00000 23.62227 54.30052 11.34904 44.41991
 (between_SS / total_SS =  65.9 %)
#
# The first part tells us where (in this scaled space) the five cluster centers are; the
# clustering part tells us which state goes with which cluster; and the last part tells us
# how spread out the clusters are. The first cluster contains only Alaska, so it has no spread.
#
# Cluster 2 seems reasonable....
#
state.km$cluster[state.km$cluster == 2]
       Alabama       Arkansas        Georgia       Kentucky      Louisiana 
             2              2              2              2              2 
   Mississippi     New Mexico North Carolina South Carolina      Tennessee 
             2              2              2              2              2 
 West Virginia 
             2 

#
# Just to draw a cute picture:
#
library (maps)
map ("state")
map ("state", state.name[state.km$cluster == 1], col = "darkgreen", fill = T, add=T)
map ("state", state.name[state.km$cluster == 2], col = "blue", fill = T, add=T)
map ("state", state.name[state.km$cluster == 3], col = "orange", fill = T, add=T)
map ("state", state.name[state.km$cluster == 4], col = "white", fill = T, add=T)
map ("state", state.name[state.km$cluster == 5], col = "pink", fill = T, add=T)

#
# What's a good value for k? Let's try 2, 3, ..., 15 clcusters.
#
wss <- numeric (length (2:15))
names (wss) <- 2:15 # vector names
for (i in 2:15)  {
    out <- kmeans (scale (state.x77), i, nstart=1000, iter.max=100)
    wss[as.character (i)] <- out$tot.withinss
}
plot (2:15, wss, type = "b") # where is the knee??





