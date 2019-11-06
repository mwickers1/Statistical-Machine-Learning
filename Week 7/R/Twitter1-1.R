#
# Tweet reader starting point.
#
require (jsonlite)
#
# Open input file. Use "r" for unzipped, "rb" for gzipped (this may
# not matter). gzfile() gives warnings on readLines() but seems to work.
# "fi" = "file input": "if" for "input file" would be a bad name.
#
fi <- file ("tweets.20130201_055232.10000lines", "r")

fo <- "" # Use stdout; replace with file name if needed
i <- 1
#
# Write out header. This is the only "write" without append=TRUE, so this
# command over-writes anything that was already in outfile.
#
cat ("Lang\tDate\tTZ\tLong\tLat\tId\tText\n", file = fo)

while (i < 100) {
  myline <- readLines (fi, 1)
  if (length (myline) == 0) # no line retrieved means EOF (end of file)
    break
  if (length (myline) == 1 && nchar (myline) == 0) # some lines are empty; skip 'em
    next 
  a <- try (txt <- fromJSON (myline))
  if (class (a) == "try-error") {
    cat ("Line", i, "was an error\n") # to stdout
    i <- i + 1
    next
  }
  if (names (txt)[1] == "delete") { # silently skip deletes for now
    ##      cat ("Line", i, "was a delete\n") # to stdout
    i <- i + 1
    next 
  }
  #
  # Print out the text. The system should know if this is UTF-8.
  cat (txt$text, "\n")
  
  #
  # Tasks: (1) find the "created_at" date and convert it into a POSIX-style
  # date (object of class POSIXlt), possibly with strptime(); then convert
  # to text. (2) Find the "text" and make it printable. In particular, convert
  # new-lines, tabs, and carriage returns (\r) to something else. (3) Pull
  # out the language ("lang") and timezone ("time_zone") fields (the second one
  # might be missing). (4) Pull out coordinates, if supplied; if found, the
  # coordinates object contains a vector named "coordinates" with long and
  # lat.
  #
  i <- i + 1
}
close (fi)
