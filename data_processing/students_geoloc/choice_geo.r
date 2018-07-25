library(svMisc)
library(DBI)
library(aws.s3)
library(tidyverse)
library(stringr)

source('/Volumes/GoogleDrive/My Drive/eScience/projects/dssg2018/cred.txt')
#Might need to set systemenv

rawAWSdata <- function(filename) {
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
  unite(Address,Address, CityStateZip, sep = " ", remove = FALSE) #%>%
  #slice(1:10)
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
  Sys.sleep(0.2)  # API only allows 50 requests per second
  out
}

# Set up loop

#Initialize
geo_data$LAT<-NA
geo_data$LON<-NA
g_add=list()

old <- Sys.time() # get start time
for (i in 1:nrow(geo_data)) {
  g_add <- geocodeAddress(geo_data$Address[i])
  geo_data$LAT[i] <- g_add[1]
  geo_data$LON[i] <- g_add[2]

  if (i == nrow(geo_data)) cat("Done!\n")
}
new <- Sys.time() - old # calculate difference
print(new)

############
# Addresses that have "#" in the address string will retun NA, remove "#" and re-run.
############
names(geo_data)
miss_ll <- geo_data %>%
            filter(is.na(LAT) | is.na(LON) ) %>% 
            mutate_at(vars(Address), funs(str_replace_all(., "#", "")))

g_add=list()

old <- Sys.time() # get start time
for (i in 1:nrow(miss_ll)) {
  g_add <- geocodeAddress(miss_ll$Address[i])
  miss_ll$LAT[i] <- g_add[1]
  miss_ll$LON[i] <- g_add[2]

  if (i == nrow(miss_ll)) cat("Done!\n")
}
new <- Sys.time() - old # calculate difference
print(new)

# Create vector of students that were re-run 
missing_s <- miss_ll$StudentNumber

# remove from original 
geo_data2 <- geo_data %>%
                filter(!StudentNumber %in% missing_s)

#append for complete data table
student_location = bind_rows(geo_data2,miss_ll)
#################
# Store in RDS
#################