# Set up text for reg. exp. This is one lone line with embedded new-line characters.
txt <- "Here's a line with a 1 and a backslash: \
I knew a woman age 34 with an IQ of 165 in 2013 whose Zip was 93950.
not every line starts with a capital letter; I know this
My brother owes me $400, which is $(20^2).
An ellipsis...makes you pause
Years ago, my mother lived at 241 E. 58th. St.
NPS's Zip code is 93943"

# Split at new lines. This is a regular expression!
txt <- strsplit (txt, "\n")[[1]]
#
# Straightforward regular expressions
#
grep ("I", txt)                  # indices of lines with I
grep ("I", txt, value = T)       # text of lines with I
grepl ("I", txt)                 # logical showing lines with I
grep ("^I", txt, value = T)      # lines starting with I
grep ("\\.", txt, value = T)     # lines with a dot
grep ("\\.\\.", txt, value = T)  # lines with dot-dot
grep ("[\\^]", txt, value = T)   # lines with a caret
#
# Character classes and repetition
#
grep ("^[[:lower:]]", txt, value = T)   # lines that start with lower-case letter
grep ("\\.{3}", txt, value = T)         # lines with three dots
grep ("[[:digit:]]", txt, value = T)    # lines with any digit
grep ("[2-9]", txt, value = T)          # lines with any digit 2-9
grep ("[68]", txt, value = T)           # lines with a 6 or an 8
grep ("[[:digit:]]{3}", txt, value = T) # lines with (>=3) consecutive digits
grep ("[^[:digit:]]", txt, value = T)   # lines with any non-digit
grep ("^[^[:digit:]]$", txt, value = T) # lines with exactly one non-digit
grep ("^[^[:digit:]]+$", txt, value = T) # lines with all non-digits

grep ("0.*0", txt, value = T) # lines with two possibly separated 0's
grep ("0.+0", txt, value = T) # lines with two separated 0's 
grep ("0.+0", "My brother owes me $400.") # FYI
#
# Word example
#
grep ("\\<[[:digit:]]\\>", txt, value = T) # lines with a word made of digits
#
# Where are the digits? This fings the *first* sequence in each element
#
(reg.out <- regexpr ("[[:digit:]]+", txt))
#
# Extracting those sequences: the harder way
#
substring (txt, reg.out, reg.out + attr (reg.out, "match.length") - 1)
#
# The easier way (?)
#
regmatches (txt, reg.out) # those strings w/o #'s are excluded
#
# Globally
#
(greg.out <- gregexpr ("[[:digit:]]+", txt))
regmatches (txt, greg.out) # those without are excluded
#
# Greedy example
#
regmatches (txt, regexpr ("[[:alpha:] ]+", txt))           # Maximum {word + space}
regmatches (txt, regexpr ("[[:alpha:]|[:punct:] ]+", txt)) # Maximum {wd/space/punct}
regmatches (txt, regexpr ("[[:alpha:] ]+?", txt))          # First {word or space}
regmatches (txt, regexpr ("\\<[[:alpha:]]+\\>", txt))      # First wd made of letters

thing <- "123 is easy as ABC"
regmatches (thing, regexpr ("\\<[[:alpha:]]+\\>", thing))  # First wd made of letters

# To pick up the first word of any sort...
thing <- c(txt, thing)
regmatches (thing, regexpr ("\\<[^[:space:]]+\\>", thing))  # First wd of non-spaces
