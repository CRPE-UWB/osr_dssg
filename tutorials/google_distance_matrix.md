# Using the [Google Distance Matrix](https://developers.google.com/maps/documentation/distance-matrix/intro) with R

## The API call
The API call looks like this"
```
https://maps.googleapis.com/maps/api/distancematrix/outputFormat?parameters
```
and takes these parameters:
- `mode` (defaults to driving) but can take:
    - walking
    - bicycling
    - transit
        - where you can specify transit mode (bus, train, etc.)
        - routing preferences (less_walking, fewer_transfers)
- `duration_in_traffic` length of time it takes to travel this route.
- `departure_time` when you want to test this route
- `origin` you can pass latitude and longitude coordinates or an address string.
- `destination` same as origin
- example: `origins=41.43206,-81.38992|-33.86748,151.20699`
    - You seperate locations using the `|`
- `key` Is your unique API key. 

Example call:

```
https://maps.googleapis.com/maps/api/distancematrix/xml?origins=&destinations=&mode=&units=imperial&departure_time=&sensor=false&key=
```
*see [documentation](https://developers.google.com/maps/documentation/distance-matrix/intro) for full parameter specifications*

## You can create a function that returns the results for one case and use it to loop through a column of origins and destinations

```r
gdist_api <- function(origin,destination,mode,depart){
    xml.url <- paste0('https://maps.googleapis.com/maps/api/distancematrix/xml?origins=',origin,'&destinations=',destination,'&mode=',mode,'&units=imperial&departure_time=',depart,'&sensor=false&key=',apikey)
    xmlfile <- xmlParse(getURL(xml.url))
    dist <- xmlValue(xmlChildren(xpathApply(xmlfile,"//distance")[[1]])$text)
    distance <- as.numeric(sub(" mi","",dist))
    dura <- xmlValue(xmlChildren(xpathApply(xmlfile,"//duration_in_traffic")[[1]])$value)
    duration <- as.numeric(dura)
    return(c(distance, duration))
}
```

## Data we have should look like this:
| census_block_id| census_block_lat | census_block_lng | resource_id | resource_lat | resource_lng |
|------------|---------|--------|-------|-----|-----|
| 1| 43.4848| 102.8776| 34| 43.6464| 102.8087|
| 2| 43.2313| 102.9034| 34| 43.6464| 102.8087|
| 3| 43.6726| 102.5434| 34| 43.6464| 102.8087|
| 4| 43.8625| 102.8874|34| 43.6464| 102.8087|

### Where we have census_block centroids and we want to determine their distance in relation to various resources in the city using different modes of travel.  We will have to determine a set of "resources to use" against all census blocks.  (I am open to re-thinking this as we might be pulling a lot of requests and this cost $$)

## Data Processing 
### We want to pass two parameters to our function with the `origin` and `destination` using the `lat` and `lng` columns, in the form `origin="41.43206,-81.38992"` and `destiniation="-33.86748,151.20699"`. We can combine columns to match that form.  (might be a better way...)

You can add two new columns `OCoords` and `DCoords` for origin and destination in the following way.

```r
data <- unite(data, OCoords, census_block_lat , census_block_lng, sep = ",", remove = F)
data <- unite(data, DCoords, resource_lat, resource_lng, sep = ",", remove = F)
```
### You can set up the loop like so:
```r
for(i in 1:nrow(data)){

origin <- data$OCoordsi]
destination <- data$DCoords[i]

  drive.temp <- try(gdist_api(origin=origin, destination=destination,mode='driving'), TRUE)
  trans.temp <- try(gdist_api(origin=origin, destination=destination,mode='walking'), TRUE)

  data$drive.dist[i] <- drive.temp[1]
  data$drive.dura[i] <- try(drive.temp[2] / 60, TRUE)
  data$trans.dist[i] <- trans.temp[1]
  data$trans.dura[i] <- try(trans.temp[2] / 60, TRUE)
  
  Sys.sleep(1) 
  
}
```
### loop guide:
```r
for(i in 1:nrow(data)){
```
### go through each origin/destination in dataset
```r
origin <- data$OCoordsi]
destination <- data$DCoords[i]
```
### There may be no valid directions, rather than end the whole process; the API will simply produce an error code where the distance and durations measures should be `try()` will let you move on to the next record
you can set up multiple modes to run
```r
  drive.temp <- try(gdist_api(origin=origin, destination=destination,mode='driving'), TRUE)
  trans.temp <- try(gdist_api(origin=origin, destination=destination,mode='walking'), TRUE)
```
  ### assign distance & time as new variables (default time is in seconds, so dividing by 60 to get minutes)
  ```r
  data$drive.dist[i] <- drive.temp[1]
  data$drive.dura[i] <- try(drive.temp[2] / 60, TRUE)
  data$trans.dist[i] <- trans.temp[1]
  data$trans.dura[i] <- try(trans.temp[2] / 60, TRUE)
```
### rest for 1s before moving onto next record to prevent overloading API
```r
  Sys.sleep(1) #might be able to do a smaller interval but will need to research
  
}
```
### Make sure you test things out with a small data sample before you schedule this.  We might need to set up some instances to run on aws or break up the data set and have each one of us be responsible to run it.  







