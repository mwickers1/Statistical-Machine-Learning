##OS4118 Exercise 1
##Melissa Wickers
##October 8, 2019

##Principal Component Analysis

vocab <- scan(file="vocab.kos.txt", what = 'character')
docword <- read.table('docword.kos.txt', skip = 3)


kos <- matrix(nrow = 3430, ncol= 6906) ##create kos matrix.
kos[is.na(kos)] <- 0 ##replace NAs with 0.

docword <- as.matrix(docword) ##convert docword to matrix.
index = docword[,1:2] ##set index to first two columns of docword.

kos[index] <- docword[,3] ##Fill in kos cells that correspond to the index with the 3rd column of docword
colnames(kos) <- vocab ##change names of columns to the list of words in vocab
View(kos)


kos.pr <- prcomp(kos) ##PCA of kos matrix.
plot(kos.pr) ##scree plot


kos.pr2 <- prcomp(kos[,1:4]) ##based on scree, choose only 4 PCs
plot(kos.pr2) ##scree plot

summary(kos.pr2)


scores1 <- kos.pr$x ##scores for full kos
scores2 <- kos.pr2$x ## score for 4 PCs

##plot scores
plot(scores1)
plot(scores2)

rotation <- as.data.frame(kos.pr$rotation) ##matrix of variable loading.
rotationPC1 <- rotation[with(rotation,order(-PC1)),] ##ordered data set by PC1 descending.
View(rotationPC1) ##top 10 row names are the words that have largest loading

rotationPC2 <-rotation[with(rotation,order(-PC2)),] ##ordered data set by PC2 descending.
View(rotationPC2)##top 10 row names are the words that have largest loading



##1. I will keep 4 PCs in order to keep 90% of the variance.
##2. The scores have a negative structure. It is more apparent with the full matrix than with only 45 PCs.
##3. The top 10 most important words in PC1 are: iraq, american, blades, meteor, intelligence, dean, delay, bghdad, soldiers, ghraib.
##   The top 10 most important words in PC2 are: november, senate, governor, account, primary, electoral, races, contact, parecommend, duderino.