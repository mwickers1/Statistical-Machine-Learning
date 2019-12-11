library(dplyr)
library(spatstat)

data <- read.csv('Data/311_Cases.csv', quote = "\"", comment = "", stringsAsFactors = F)
shapes <- read.csv('Data/sf_polygon.csv', stringsAsFactors = F)

data <- data %>%
  filter(Latitude != 0)

dataPE <- data %>%
  filter(Category == 'Parking Enforcement')

window <- owin(xrange = c(min(shapes$x), max(shapes$x)), yrange = c(min(shapes$y), max(shapes$y)))

pp <- ppp(data$Longitude, data$Latitude, window = window, poly = 'p')
plot(pp)
plot(envelope(pp))

source('R/writekml.r')

write.kml (dataPE, "long", "lat", outfile = "myfile.kml", icon.num = 1)
