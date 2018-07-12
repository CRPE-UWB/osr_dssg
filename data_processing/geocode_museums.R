# This file records how our museum location data was geocoded.
# Author: Sree
# Editor: Kellie

# Function to get the lat and long using google API
geocodeAddress <- function(address) {
  require(RJSONIO)
  full <- paste(address)
  url <- "https://maps.google.com/maps/api/geocode/json?address="
  url <- URLencode(paste(url, full, '&sensor=false&key=','AIzaSyAHW3TJFoPOIqXl9-lu4Wz928vu38kUCxE', sep = ""))
  x <- fromJSON(url, simplify = FALSE)
  if (x$status == "OK") {

      out <- c(x$results[[1]]$geometry$location$lat,
               x$results[[1]]$geometry$location$lng)
    } else {
      out <- NA
  }
  Sys.sleep(0.05)  # API only allows 50 requests per second
  out
}

# Read in museum addresses, which were manually extracted from Google Maps.
museumdata = read.csv(file = "C:/Users/Sreekanth/Desktop/DSSG Project/museum_locations.csv",  
                      na.strings = "")

# Initialize
g_add=list()

# Run the geocoding
old <- Sys.time() # get start time
for (i in 1:nrow(museumdata)) {
  g_add <- geocodeAddress(museumdata$address[i])
  museumdata$lat[i] <- g_add[1]
  museumdata$long[i] <- g_add[2]

  if (i == nrow(museumdata)) cat("Done!\n")
}
new <- Sys.time() - old # calculate difference
print(new)

museumdata_final = museumdata