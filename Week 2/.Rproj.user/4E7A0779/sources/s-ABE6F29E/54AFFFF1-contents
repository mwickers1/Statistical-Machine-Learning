library(dplyr)
library(cluster)
library(treeClust)
library(ggplot2)
library(Rtsne)


spliceData <- read.fwf("data/splice.data.txt", width = c(2,37, rep(1, 60)))
spliceData <- spliceData[, -2]

filteredData <- spliceData[-which(apply (spliceData[,-1], 1, function (x) 
  any(!x %in% c("A", "C", "T", "G"))) == T),]

sampledData <- sample_n(filteredData, 1000)
class <- sampledData[,1]
sampledData <- sampledData[,-1]

gower <- daisy(sampledData, 'gower')

treeClust <- treeClust.dist(sampledData, d.num = 4)

gower2 <- cmdscale(gower)
plot(gower2, col = class)
 
treeClust2 <- cmdscale(treeClust)
plot(treeClust2, col = class)

gower3 <- Rtsne(gower)
plot(gower3$Y, col = class)

treeClust3 <- Rtsne(treeClust)
plot(treeClust3$Y, col = class)
