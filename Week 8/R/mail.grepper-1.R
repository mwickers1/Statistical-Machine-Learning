em <- scan ("weird.email.txt", what = "", sep="\n") # blank lines skipped
#
# Letter, then {letter, number, dots}; then @-sign; then either word-dot-word,
# or word-dot-word-dot-word. 
#
grep ("[[:alpha:]]([[:alnum:]]|\\.)*@([[:alnum:]]+\\.){1,2}[[:alnum:]]+", em, v = T)
#
# Notice that included a few we don't want. Why?
#
# Since "\w" is synomymous with [[:alnum:]]...

grep ("\\w(\\w|\\.)*@(\\w+\\.){1,2}\\w+", em, value = T)

# If we knew each email was on its own line, we could specify that.
#
grep ("^[[:alpha:]]([[:alnum:]]|\\.)*@([[:alnum:]]+\\.){1,2}[[:alnum:]]+$", em, v = T)
#
# Omit that double-dot one...
#
mails <- grep ("^[[:alpha:]]([[:alnum:]]|\\.)*@([[:alnum:]]+\\.){1,2}[[:alnum:]]+$", em, v = T)
setdiff (mails, grep ("\\.\\.", em, value = T))
#
# I found this page amusing and the site generally helpful
#https://www.regular-expressions.info/email.html