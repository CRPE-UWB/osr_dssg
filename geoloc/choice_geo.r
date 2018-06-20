library(svMisc)
library(DBI)
library(aws.s3)
library(tidyverse)

source('/Volumes/GoogleDrive/My Drive/eScience/projects/dssg2018/cred.txt')
#Might need to set systemenv

#Sys.setenv("AWS_ACCESS_KEY_ID" = access_key,
#          "AWS_SECRET_ACCESS_KEY" = secret_key,
#           "AWS_DEFAULT_REGION" = "us-west-2")


#data <- s3read_using(FUN = read.csv, object = "s3://data.all/City/Denver/DPS/CSVs/DPS_Choice_1314-1718.csv")

rawAWSdata <- function(filename) {
  location = 's3://data.all/City/Denver/DPS/CSVs/'
  s3read_using(FUN = read.csv, object = paste(location, filename, sep=''))
}


data <- rawAWSdata('DPS_Choice_1314-1718.csv')

str(data)

is.na(data) <- data == ""
names(data)

geo_data <- data %>%
  select(StudentNumber,Address,CityStateZip) %>%
  distinct(.) %>%
  filter(!is.na(Address)) %>%
  unite(Address,Address, CityStateZip, sep = " ", remove = FALSE) %>%
  slice(1:3)
#test$comb <- full <- paste(test$Address, test$CityStateZip, sep=" ")

geo_data$Address <- as.character(geo_data$Address)

str(geo_data)
##
#Google API call
geocodeAddress <- function(address) {
  require(RJSONIO)
  full <- paste(address)
  url <- "https://maps.google.com/maps/api/geocode/json?address="
  url <- URLencode(paste(url, full, '&sensor=false&key=',apikey, sep = ""))
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
#Initialize
geo_data$LAT<-NA
geo_data$LON<-NA

g_add=list()
for (i in 1:nrow(geo_data)) {
  g_add <- geocodeAddress(geo_data$Address[i])
  geo_data$LAT[i] <- g_add[1]
  geo_data$LON[i] <- g_add[2]
}

