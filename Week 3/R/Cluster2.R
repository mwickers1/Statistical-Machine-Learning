#
# Clustering examples, cont'd. Use the "cluster" package.
#
library (cluster)

########################

# Now let's try pam, also with 5 clusters, also standardizing the data.
#
pam (state.x77, 5, stand=T) # equivalent to pam (daisy (state.x77, stand=T))
Medoids:
           ID Population Income Illiteracy Life Exp Murder HS Grad Frost   Area
Tennessee  42       4173   3821        1.7    70.11   11.0    41.8    70  41328
Alaska      2        365   6315        1.5    69.31   11.3    66.7   152 566432
California  5      21198   5114        1.1    71.71   10.3    62.6    20 156361
Nebraska   27       1544   4508        0.6    72.60    2.9    59.3   139  76483
Indiana    14       5313   4458        0.7    70.88    7.1    52.9   122  36097
Clustering vector:
       Alabama         Alaska        Arizona       Arkansas     California 
             1              2              1              1              3 
...
#
# A medoid is always one of the observations. How well does pam agree with kmeans?
#
  
state.pam <- pam (state.x77, 5, stand=T)

table (state.pam$cluster, state.km$cluster)
   
     1  2  3  4  5
  1  0 11  1  0  0
  2  1  0  0  0  0
  3  0  0  0  3  0
  4  0  0  3  0 15
  5  0  0 13  0  3
#
# Answer: pretty well. Check out the pretty pictures.
#
plot (state.pam) # that first picture shows two principal components

library (fpc)
pout <- pamk (scale (state.x77), 2:15, stand=T) 
plot (pout$crit, type = "b") # avg. silhouette width criterion says 2
pout <- pamk (scale (state.x77), 2:15, criterion = "ch", stand=T) 
plot (pout$crit, type = "b") # "ch" criterion says 2 also.

#
#
# ---------------------------------------------
# Hierarchical models
# ---------------------------------------------
#
# Here's agnes, which builds from the bottom up.
#
state.ag <- agnes (state.x77, stand=T)
plot (state.ag) # pretty pictures
#
# "cutree" assigns rows to clusters. Let's ask for five clusters.
#
cutree (state.ag, 5)
 [1] 1 2 3 1 4 3 3 3 3 1 3 3 3 3 3 3 1 1 3 3 3 3 3 1 3 3 3 3 3 3 1 4 1 3 3 3 3 3
[39] 3 1 3 1 5 3 3 3 3 1 3 3

table (cutree (state.ag, 5))

 1  2  3  4  5 
11  1 35  2  1 
#
# In this data set, agnes wants to cut off outliers (Alaska, Texas, etc.) and assign
# them to their own small clusters. Diana, the divisive algorithm, does something similar.
#
state.di <- diana (state.x77, stand=T)
table (cutree (state.di, 5))

 1  2  3  4  5 
14  1  3 31  1 
plot (state.di) # pretty pictures!
#
# How well do these agree?
# 
table (cutree (state.ag, 4), cutree (state.di, 5))
   
     1  2  3  4  5
  1 11  0  0  0  0
  2  0  1  0  0  0
  3  3  0  0 31  1
  4  0  0  3  0  0

#
# Answer: pretty well, but on the other hand they both produce one big cluster
# with more than half the observations.
#
#
