#
# Table Example
#
# install.packages ("XML") # one time
# install.packages ("htmltab") # one time
library (XML)
library (htmltab)

link1 <- "https://www.eia.gov/dnav/pet/pet_cons_wpsup_k_w.htm"
link2 <- "https://www.eia.gov/petroleum/gasdiesel/"

readHTMLTable (link1)
htmltab (link1)                             # neither of these work well
(egad <- scan (link1, what = "", sep="\n")) # this works...but what a pain
readHTMLTable (egad)                        # progress

readHTMLTable (link2)
(zip <- htmltab (link2))       # better -- but needs work

names (zip) <- c("Loc", zip[2, 2:4], "ChgWeek", "Space", "ChgYear")
zip$Space <- NULL
zip[-(1:2),] # custom clean-up