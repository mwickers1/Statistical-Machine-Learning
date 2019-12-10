# install.packages ("cleanNLP")
library (cleanNLP)
library(dplyr)
library(stringr)
library(tm)
cnlp_init_udpipe ()
cnlp_annotate ('This is a test')
#
# Let's see if we have all those abstract files in the current directory.
# That have names that start 1, then 4-8, then some stuff, ending in
# dot, t, x, t. This looks like a job for regular expressions!
#
length (dir(path = 'Data/',pattern = "^1[4-8].+\\.txt")) # 181
abs <- dir(path = 'Data/',pattern = "^1[4-8].+\\.txt") # file names
#
# Creating a simple corpus from all the documents is straightforward.
#
or.corpus1 <- VCorpus(DirSource ("Data/", pattern = "^1[4-8].+\\.txt", mode = "text"))

#
# Let's annotate one document -- say, number 15
#

vec <- character()

for (i in 1:length(abs)) {
  
  intxt <- paste(scan(paste0('Data/',abs[i]), what = "", sep="\n"), collapse= " ")
  annot.out <- cnlp_annotate(intxt)
  #
  # Okay. the "token" element of the "annot.out" item lists all
  # the tokens -- words -- in the document. Your jobs are:
  # 1.) Extract just the tokens for which upos is PROPN, NOUN, or VERB.
  # 2.) Now extract just the lemmas from these tokens.
  # 3.) Convert the lemmas to lower-case...
  # 4.) and paste() them together using collapse = " " to create one long
  # document-like string with just the 'important' words.
  # 5.) If you can do that for one document, you can do it for all of them.
  # Create an empty character vector of the proper length, and insert the
  # processed abstracts into it. At the end of that operation you'll have
  # a vector of 181 character strings.
  
  filterAnno <- annot.out$token %>%
    filter(str_detect(upos,c('NOUN', 'PROPN', 'VERB')))
  
  filterAnno$lemma <- tolower(filterAnno$lemma)
  lemma <- paste(filterAnno$lemma, collapse = ' ')
  
  vec <- c(vec, lemma)
}


# Now create the new corpus.
or.corpus2 <- VCorpus(VectorSource(vec))
or.corpus2<-tm_map (or.corpus2, removeWords, c(stopwords ("english"), 'data', 'model', 'develop', 'thesis','study', 'use'))

# Use DocumentTermMatrix from library tm to construct the DTM.

dtm <- DocumentTermMatrix(or.corpus2)
rowTotals <- apply(dtm, 1, sum)
dtm <- dtm[rowTotals>0,]

library (topicmodels)
library(tidytext)
library(ggplot2)
# Use LDA from topicmodels to perform LDA.

lda <- LDA(dtm, k = 4)



topics <- tidy(lda, matrix = "beta")

library(ggplot2)

top_terms <- topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()




