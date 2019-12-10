#
# N-gram example
#
library (tm) # for "crude" 
library (tau) # for n-gram

data (crude)
#
# Convert crude to text
#
outlist <- unlist (tm_map (crude, as.character))
names (outlist) <- NULL

textcnt (outlist, n = 2, split = "[[:space:][:punct:][:digit:]]+", 
    method = "string", decreasing=T)
#
# Bigger example. This took three minutes on my laptop
# 
huge <- scan ("text8", what="") # 17,000,000 words
date()
egad <- textcnt (huge, n = 3, split = "[[:space:][:punct:][:digit:]]+", 
            method = "string", decreasing=T)
date()
egad[1:10] # these may be years?
grep ("president", names (egad), value = T)[1:10]
grep ("\\<kung\\>", names (egad), value = T)[1:10]
#
# Ridiculous text generation example. Start with "In a"
#
cat ("In a ")
current.minus.1 <- "in"
current <- "a"
for (i in 1:6) {
    search <- paste0 ("^", current.minus.1, " ", current, " ")
    vec <- egad[grep (search, names (egad), v = T)]
    if (length (vec) == 0) # nothing found
        vec <- c("the" = 1)
    probs <- vec / sum (vec)
    if (length (probs) == 1)
        new <- vec
    else
        new <- sample (vec, 1, prob = probs)
    new <- strsplit (names (new), " ")[[1]][3]
    cat (" ", new)
    current.minus.1 <- current
    current <- new
}

# Examples: In a one often sees the injustice
#   but  even  in  countries  close  to  his  one  six  four  zero 
# eight  zero  zero  zero  one  nine  eight  five  zero  zero  four  
# the  character  of...


