#
#
#
# Check out descriptions of English-language models at 
# https://universaldependencies.org/treebanks/en-comparison.html
#
require (udpipe)
mod <- udpipe_download_model (language = "english-ewt")
eng <- udpipe_load_model (mod)

lva <- scan ("llamas.vs.alpacas.txt", what="", sep="\n")
#
# This result is in CONLL-U ("Computational Natural Language Learning")
# format.
#
eout <- udpipe (lva, object = eng)
uout <- udpipe_annotate (eng, lva)

library (cleanNLP)
#
# Udpipe doesn't do entity recognition directly, but cleanNLP will.
#
# cnlp_init_udpipe ("english-ewt") # but instead let's use...

# cnlp_download_corenlp () # one time
# This needs to be done once per session. The higher the level,
# the more annotation can be done. The default is 2, but let's
# ask for 3, which provides co-references.
options(java.parameters = "- Xmx1024m")  # increase memory for Java
# This took about 1 minute on my machine
cnlp_init_corenlp ("en", anno_level = 3) # once per R session?

anno.out <- cnlp_annotate (lva, backend = "coreNLP") 
                          # argument "backend" sets itself by default

lva2 <- iconv (lva, to = "UTF-8") # needed if there's non-UTF-8 input
anno.out <- cnlp_annotate (lva2) 
cnlp_get_entity (anno.out)

txt <- "The Mona Lisa is a 16th century oil painting created by Leonardo. It's held at the Louvre in Paris."
anno.out <- cnlp_annotate (txt) 
cnlp_get_entity (anno.out)
#
# How about some co-references?
#
txt <- "Ruth Bader Ginsberg is an Associate Justice of the U.S. Supreme
  Court. Born March 15, 1933 in Brooklyn, NY, she studied at Cornell Univerity
  and Harvard and Coumbia Law Schools. The New Yorker spent two years in
  Sweden before returning in 1963 as a professor at Rutgers. Active in the
  ACLU, RGB is credited with advancing women's rights under the Equal Protection
  clause of the Constitution. She was nominated by Jimmy Carter to a seat
  on the US Court of Appeals for the DC Circuit and elevated to the Supreme
  Court in 1993. The mother of two has a collection of lace jabots."
rgb.out <- cnlp_annotate (txt) 
cnlp_get_entity (rgb.out) # not entirely satisfying

#
# Let's look at common words in the "crude" data set from tm.
#
library (tm)
data (crude)
#
# Use the tm_map function to loop
#
yout <- lapply (1:length (crude), function (i) {
    cat ("Operating on item ", i, "\n")
    txt <- crude[[i]]$content
    thing <- cnlp_annotate (txt)
    return (list (token = data.frame (Src = i, thing$token),
                  coref = data.frame (Src = i, thing$coreference)))
})
#
# Combine token data frames
#
crude.tokens <- lapply (yout, function (x) x$token)
crude.tokens <- do.call (rbind, crude.tokens)
#
# What are some common nouns?
#
sort (table (crude.tokens$lemma[crude.tokens$upos == "NOUN"]), dec=T)[1:30]
#
# We can use the built-in txt_freq() function to make a pretty picture.
#
crude.freq <- txt_freq (crude.tokens$lemma[crude.tokens$upos == "NOUN"])

# Add a little room on the left, set axis labels horizontal. I used rev()
# to make the longest bar be at the top of the picture.
#
par (mar = c(5.1, 6.1, 2.1, 2.1), las = 1)
barplot (rev (crude.freq$freq[1:20]), horiz = T, col = "dodgerblue",
    names.arg = rev (crude.freq$key[1:20]))

crude.vfreq <- txt_freq (crude.tokens$lemma[crude.tokens$upos == "VERB"])
par (mar = c(5.1, 6.1, 2.1, 2.1), las = 1)
barplot (rev (crude.vfreq$freq[1:20]), horiz = T, col = "dodgerblue",
    names.arg = rev (crude.vfreq$key[1:20]))
# Less interesting...

