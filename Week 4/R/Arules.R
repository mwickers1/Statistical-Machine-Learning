#
# Adult stuff, mostly from the help
#
require (arules)
data (AdultUCI) # make data available
help (AdultUCI)
#
# What does the original data look like?
#
AdultUCI[1:10,]
# Example table
table (AdultUCI$native-country, AdultUCI$income) # why doesn't this work?
names (AdultUCI) <- make.names (names (AdultUCI))
table (AdultUCI$native.country, AdultUCI$income) # Does country predict high income?
nat <- as.character (AdultUCI$native.country)
# Western countries, other than US
west <- c("Canada", "Germany", "England", "Ireland", "Italy", "France")
nat[!nat %in% c("United-States", "Canada", west)] <- "Other"
nat[nat %in% west] <- "OtherWest"
(inctbl <- table (nat, AdultUCI$income))
cbind (inctbl, largeRate = inctbl / rowSums (inctbl)) # hmmm
#
# Okay, let's build rules. "Adult" is the transactional data set
#
Adult
itemFrequencyPlot(Adult, topN=20) 
adult.sets <- eclat (Adult) # construct frequent itemsets
inspect (adult.sets[1:10]) # lots of overlap
adult.rules <- apriori (Adult, 
	parameter = list(supp = 0.5, conf = 0.9, target = "rules")) # from help

inspect (sort (adult.rules, by = "lift")[1:10])
#
# There are just a lot of US-ers here, but numbers 9 and 10 might say something
#

#
# Tunnels stuff. The original data is fun to look at, but messy.
#
tunnels <- read.delim ("tunnels.tsv", row.names = NULL)
library (maps)
map ("state", c("california", "arizona"))
# map ("county", c("california,kern"), add=T, col = "red", fill = T)
points (-1 * tunnels$East, tunnels$North, col="red")

#
# Here's a cleaner version.
#
tunnels <- read.delim ("tunfix.tsv", row.names = NULL)


#
# Remove Numeric "East" and "North" columns
#

tun.trans <- as (tunnels[,!is.element (names (tunnels), c("East", "North"))],
                  "transactions") 

itemFrequencyPlot(tun.trans, topN=20) # shows each categorical level's frequency

trules <- apriori (tun.trans, 
                  parameter=list (minlen=2, maxlen = 2, support=0.5)) 
#
# Rules are not in any particular order. Here are 20 of them.
#
inspect (trules[1:20])
#
# Let's look at the two rules "No Lights -> No Vent" and "No Vent ->
# No Lights." 
#
# Computations: apriori defines "support" a little differently than I do.
# They use the probability of both antecedent and consequent. So for the 
# rule "No Lights -> No Vent," the support is the proportion of all
# tunnels with both No Lights and also No Vent.
#
table (tunnels$Lights, tunnels$Vent)
#
# It's 88/115, which is 0.765. The confidence is the proportion among items
# with no lights that also have no vent. That's 88/88 -- 1.0.
# To me, the lift is the ratio 1 / .765, but
# this algorithm computes lift another way:
# Pr (both) / Pr (one) Pr (the other)
# = (88/115)/( 88/115 * 95/115) = 1.211.
#
#
# For the converse, the support is the same 88/115. This time the
# confidence is 88/95 = .926. The lift is the same under apriori's
# computation.

#
# Top ten rules by lift
#
inspect (sort (trules, by = "lift")[1:10])

#
# Lots of rules have Dewater=N, Light=N, Vent=N, Shoring=N on the 
# left-hand side. We can prevent them from appearing there.
#
trules2 <- apriori (tun.trans, parameter=list (maxlen = 3, support=0.4, maxtime = 0), 
                     control = list (verbose = FALSE),
        appearance = list (rhs = c("Shoring=N", "Vent=N", "Lights=N", "Dewater=N")))

inspect (trules2)

