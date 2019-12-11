write.kml <- function (df, long.name, lat.name, outfile = "h:/output.kml", 
                       icon.num = 1, print.open=T, print.close=T, append=!print.open) 
{
  #
  # Write.kml: produce a KML file with "dot" placemarks.
  #
  # Arguments: df: data frame with lat and long columns.
  #     long.name: character string with column name for longitude
  #      lat.name: character string with column name for latitude
  #       outfile: destination file
  #      icon.num: currently 1-5 means red, blue, green, yellow, white
  #        append: append to outfile if TRUE, otherwise erase old outfile
  #
  # Set up constant character strings
  xml <- '<?xml version="1.0" encoding="UTF-8"?>\n'
  opentag <- '<kml xmlns="http://www.opengis.net/kml/2.2"><Document>\n'
  styletag1 <- '<Style id="dot"><IconStyle><Icon><href>'
  styletag2 <- switch (icon.num,
                       'http://maps.google.com/mapfiles/kml/paddle/red-circle-lv.png',
                       'http://maps.google.com/mapfiles/kml/paddle/blu-circle-lv.png',
                       'http://maps.google.com/mapfiles/kml/paddle/grn-circle-lv.png',
                       'http://maps.google.com/mapfiles/kml/paddle/ylw-circle-lv.png',
                       'http://maps.google.com/mapfiles/kml/paddle/wht-circle-lv.png')
  styletag3 <- '</href></Icon></IconStyle></Style>\n'
  styletag <- paste (styletag1, styletag2, styletag3, sep="")
  #
  closetag <- '</Document></kml>\n'
  start <- '<Placemark><styleUrl>#dot</styleUrl><Point><coordinates>\n'
  stop <- '</coordinates></Point></Placemark>\n'
  # Here's where the action takes place
  coo <- paste (start, df[[long.name]], ",", df[[lat.name]], ",0", stop, sep="")
  sink (outfile, append=append)
  if (print.open) {
    cat (xml)
    cat (opentag)
  }
  cat (styletag)
  cat (coo)
  if (print.close) {
    cat (closetag)
  }
  sink ()
}