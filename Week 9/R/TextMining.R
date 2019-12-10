#
# Text example
#
# install.packages (c("aplpack", "tm", "lsa", "SnowballC"))
library (tm)
#
# Make the data available. There are a lot of formats possible, and in particular
# the data needn't all fit in main memory.
#
data (crude)
crude.orig <- crude; 
cat (crude[[1]]$content)  # text held in a special form
cat (crude[[11]]$content)
# Some standard operations: remove white space, 
#
crude <- tm_map (crude, tolower); crude[[11]]
stopwords ("english")
crude <- tm_map (crude, removeWords, stopwords ("english")); crude[[11]]
crude <- tm_map (crude, removePunctuation); crude[[11]]
crude <- tm_map (crude, stripWhitespace); crude[[11]]

#
# Stemming uses the Snowball library. We need to treat the
# document as a PlanTextDocument, not just as character.
# And I'm not sure why tm_map doesn't seem to work here.
stemDocument (PlainTextDocument (crude[[11]]))[[1]]
for (i in 1:length (crude)) {
  bunnies <- stemDocument (PlainTextDocument (crude[[i]]))[[1]]
  crude[[i]] <- PlainTextDocument (bunnies)
}

cat (crude[[11]]$content) # not entirely satisfactory

#
# Create a TermDocumentMatrix. There's also a DocumentTermMatrix() function.
#
tdm <- TermDocumentMatrix (crude)
#
# It's convenient to change the document names at this point.
#
tdm$dimnames$Docs <- LETTERS[1:20]
#
# Check this out in matrix form.
#
m <- as.matrix (tdm)
#
# Compute cosine distances between documents
#
library (lsa)
#
#
co <- cosine (m)
#
# Cosine is a similarity measure, and we want distance, so...
#
co <- 1 - co
#
# cosine() returns a square matrix. We just want the lower triangle.
#
co <- co[col(co) < row (co)]

#
# Persuade agnes() that this came from daisy():
#
class (co) <- c("dissimilarity", "dist")
attr (co, "Size") <- 20
attr (co, "Metric") <- "unspecified"
attr (co, "Labels") <- LETTERS[1:20]

library (cluster)
plot (agnes (co))
#
# Which documents look similar?
#
# Let's try multidimensional scaling!
#
b <- cmdscale (co)
plot (b[,1], b[,2], type = "n")
text (b[,1], b[,2], LETTERS[1:20])
#
# Which documents look very different?
#
#
# Let's return to the tdm for a moment. Which terms appear frequently?
#
tdm # note that it's 90% sparse
findFreqTerms (tdm, 10)
#
# Remove terms which don't appear in 70% of documents
#
tdmx <- removeSparseTerms(tdm, 0.7)
#
as.matrix (tdmx) # 24 x 20
# Impose tf-idf weights
round (as.matrix (weightTfIdf(tdmx, normalize = TRUE)), 3)
#
# Do the cosine thing again?
#
co <- cosine (as.matrix (weightTfIdf(tdmx, normalize = TRUE)))
co <- 1 - co
co <- co[col(co) < row (co)]
class (co) <- c("dissimilarity", "dist")
attr (co, "Size") <- 20
attr (co, "Metric") <- "unspecified"
attr (co, "Labels") <- LETTERS[1:20]

plot (agnes (co))

b <- cmdscale (co)
plot (b[,1], b[,2], type = "n")
text (b[,1], b[,2], LETTERS[1:20])
###############################################
#
# Latent Dirichlet Analysis
#
library (topicmodels)
#
# Create the document-term matrix tdm from above; transform it
#
# dtm <- TermDocumentMatrix (crude)
dtm <- t(tdm)
#
# The "@" is like the "$" but for a different type of R object. Annoying.
#	
lout <- LDA (dtm, k = 4, control = list (alpha = 0.1)) # best fit with four topics
10^4 * lout@gamma        # which documents go with which topics?
#
# The solutions are pretty variable from run to run. Let's look at
# numbers 2, 11 and 20. Might they be about production?
#
sapply (c(2, 11, 20), function(x) return (crude[[x]]$content))

#
dim (lout@beta) # topic strengths for each word
#
# Which words are strongest for topic #2? for topic #4?
#
toptwenty <- sort (abs(lout@beta[2,]), decreasing=T)[1:20]
dimnames (dtm)[[2]][which (abs (lout@beta[2,]) > min (toptwenty))]
toptwenty <- sort (abs(lout@beta[4,]), decreasing=T)[1:20]
dimnames (dtm)[[2]][which (abs (lout@beta[4,]) > min (toptwenty))]
#
# Hmmm.
#