#
# Testing Malcolm Gladwell's claim from "Outliers" that NHL players are
# disproportionately born in Jan-Mar. He says (Jan-Mar), (Apr-Jun), (Jul-Sep),
# (Oct-Dec) birthdates are represented in proportions (.4, .3, .2, .1). Are they?
#
#----------------------------------------------
# Source: NHL.com. 
#----------------------------------------------
require (XML); require (RCurl); require (httr)
#
# Now it's time to acquire the roster pages.
#
# Read one in, just to scope 'it out. In practice we would want to look
# at a bunch of these. The getURI() function failed here, so I use 
# GET from httr. This produces a special object that we can convert
# to character with as.character().
#
# example link:
https://www.nhl.com/blackhawks/roster
#
# It'll be helpful to have this "teams" item around.
#
teams
hawx <- GET (paste0 ("http://www.nhl.com/", teams$Nickname[1], "/roster"), stringsAsFactors=FALSE)
cat (as.character(hawx))
#
# Convert to tables? I'm turning header "off" for a reason.
#
(hawxtbl <- readHTMLTable (as.character (hawx), header=F))
#
# Okay. It looks as if tables 3, 6, and 9 have the data we want -- not the
# names, but does anyone know any hockey players' names?
#
do.call (rbind, hawxtbl[c(3, 6, 9)])

results <- NULL # inefficient way to start accumulating a data frame
for (i in 1:nrow (teams)) {
    myt <- GET (paste0 ("http://www.nhl.com/", teams$Nickname[i], "/roster"), stringsAsFactors=FALSE)
    myt <- readHTMLTable (as.character (myt), header=F, stringsAsFactors=FALSE)
    this.team <- rbind (do.call ("rbind", myt[c(3, 6, 9)]))
    this.team$Team <- teams$CityShort[i]
    results <- rbind (results, this.team)
}
#
# Last year this approach didn't work, because the teams weren't consistent.
#
results <- results[,c("V2", "V6", "V7", "Team")]
names (results) <- c("Position", "Born", "Birthplace", "Team")
table (results$Team) # Look reasonable?
table (duplicated (results)) # watch out for twins
#
# The last three characters of the "Place" give the country of birth. We
# could also use everything to the right of the last comma.
#
results$Country <- substring (results$Birthplace, nchar (results$Birthplace) - 2)
results$Birthmonth <- substring (results$Born, 1, 2)
#
# Birthdates of all players, by month.
#
table (results$Birthmonth)
barplot (table (results$Birthmonth))
#
# Canadians only.
#
table (results$Birthmonth[results$Country == "CAN"])
barplot (table (results$Birthmonth[results$Country == "CAN"]))
#
# Breakdown (break down!) by quarters. Is it close to c(.4, .3, .2, .1)?
#
(qtr.counts <- tapply (table (results$Birthmonth[results$Country == "CAN"]), c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4), sum))
# 
# Much different from Gladwell's claim:
#
sum (results$Country == "CAN") * c(.4, .3, .2, .1)
#
# If we needed to do a formal hypothesis test, we could use the chi-squared
# test...but we don't. I'll do it anyway.
#
chisq.test (qtr.counts, p = c(.25, .25, .25, .25)) # nope, not believable, but...
chisq.test (qtr.counts, p = c(.40, .30, .20, .10)) # much less believable


      


