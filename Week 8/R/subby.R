#
# Substitution examples
#
prez <- "George Washington
John Adams
Thomas Jefferson
James Madison
James Monroe
John Quincy Adams
Andrew Jackson
Martin Van Buren
William Henry Harrison"; prez <- strsplit (prez, "\n")[[1]]

# Find word's #1 and #2 and switch them. Each word can be represented
# as a series of word characters \w, inside \< and \>. Surrounding each
# word in parenthese means we can use them as backreferences.
#
pattern <- "(\\<\\w+\\>) (\\<\\w+\\>)"
cat (pattern) # easier to see this way?

sub (pattern, "\\2, \\1", prez)
#
# Clearly "Van Buren" and "Quincy Adams" word differently. What if we
# hyphenate the {first/middle} guys?
#
(newprez <- sub ("John Quincy", "John-Quincy", prez))
(newprez <- sub ("William Henry", "William-Henry", newprez))
sub (pattern, "\\2, \\1", newprez) # ha! fooled by the hyphen!

# In the GNU extensions, \S means "any non-space character" = [^[:space]]
newpattern <- "(\\<\\S+\\>) (\\<\\S+\\>)" # match two words separated by a space
sub (newpattern, "\\2, \\1", newprez) # boom
#
# Example 2: switching Canadian to American dates
#
can <- "Here are some lines with DD/MM/YYYY dates.
I went to BC on 23/10/2013, and returned 1/11/2013.
7/12/1941: a date which will live in infamy
Norman Bethune, born 3/4/1890, died 11/12/1939."; can <- strsplit (can, "\n")[[1]]
#
# Change just the dates: g for globally
gsub ("([0-9]{1,2})/([0-9]{1,2})/([0-9]{4})", "\\2/\\1/\\3", can)
#