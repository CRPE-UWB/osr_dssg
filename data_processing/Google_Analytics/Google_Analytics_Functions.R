ParsePath <- function(htmlString) {
  htmlString <-(gsub("<.*?>", "", htmlString))
  htmlString <- (gsub("/search","",htmlString))
  htmlString <- (gsub("&",",",htmlString))
  htmlString <- (gsub("?","",htmlString))
  htmlString <- (gsub("/?","",htmlString))
  htmlString <- (gsub("selected","",htmlString))
  return(htmlString)
}

ReturnKeyWords <- function(x){
  check <- grepl("keywords",x)
  if(check==TRUE){
    stringcheck <- str_split_fixed(x,",",Inf)
    for (i in 1:(length(stringcheck))){
      check.i <- grepl("keywords",stringcheck[i])
      if (check.i==TRUE){
        return(stringcheck[i])}
    }}}

Returnscholarships <- function(x){
  check <- grepl("sessionAttributes",x)
  if(check==TRUE){
    stringcheck <- str_split_fixed(x,",",Inf)
    for (i in 1:(length(stringcheck))){
      check.i <- grepl("scholarships",stringcheck[i])
      if (check.i==TRUE){
        return(stringcheck[i])}
    }}}

ReturnspecialNeeds <- function(x){
  check <- grepl("sessionAttributes",x)
  if(check==TRUE){
    stringcheck <- str_split_fixed(x,",",Inf)
    for (i in 1:(length(stringcheck))){
      check.i <- grepl("specialNeeds",stringcheck[i])
      if (check.i==TRUE){
        return(stringcheck[i])}
    }}}

ReturnGifted <- function(x){
  check <- grepl("sessionAttributes",x)
  if(check==TRUE){
    stringcheck <- str_split_fixed(x,",",Inf)
    for (i in 1:(length(stringcheck))){
      check.i <- grepl("giftedStudent",stringcheck[i])
      if (check.i==TRUE){
        return(stringcheck[i])}
    }}}

ReturnBeforeCare <- function(x){
  check <- grepl("sessionAttributes",x)
  if(check==TRUE){
    stringcheck <- str_split_fixed(x,",",Inf)
    for (i in 1:(length(stringcheck))){
      check.i <- grepl("offersBeforeAfterCare",stringcheck[i])
      if (check.i==TRUE){
        return(stringcheck[i])}
    }}}


ReturnTime <- function(x){
  check <- grepl("sessionTimes",x)
  if(check==TRUE){
    stringcheck <- str_split_fixed(x,",",Inf)
    for (i in 1:(length(stringcheck))){
      check.i <- grepl("sessionTimes",stringcheck[i])
      if (check.i==TRUE){
        return(stringcheck[i])}
    }}}

ReturnMaxCost <- function(x){
  check <- grepl("maximumCost",x)
  if(check==TRUE){
    stringcheck <- str_split_fixed(x,",",Inf)
    for (i in 1:(length(stringcheck))){
      check.i <- grepl("maximumCost",stringcheck[i])
      if (check.i==TRUE){
        return(stringcheck[i])}
    }}}

ReturnMinCost <- function(x){
  check <- grepl("minimumCost",x)
  if(check==TRUE){
    stringcheck <- str_split_fixed(x,",",Inf)
    for (i in 1:(length(stringcheck))){
      check.i <- grepl("minimumCost",stringcheck[i])
      if (check.i==TRUE){
        return(stringcheck[i])}
    }}}

ReturnDistance <- function(x){
  check <- grepl("distance",x)
  if(check==TRUE){
    stringcheck <- str_split_fixed(x,",",Inf)
    for (i in 1:(length(stringcheck))){
      check.i <- grepl("distance",stringcheck[i])
      if (check.i==TRUE){
        return(stringcheck[i])}
    }}}

ReturnCategory <- function(x){
  check <- grepl("Categories",x)
  if(check==TRUE){
    stringcheck <- str_split_fixed(x,",",Inf)
    for (i in 1:(length(stringcheck))){
      check.i <- grepl("Categories",stringcheck[i])
      if (check.i==TRUE){
        return(stringcheck[i])}
    }}}

ReturnGender <- function(x){
  check <- grepl("gender",x)
  if(check==TRUE){
    stringcheck <- str_split_fixed(x,",",Inf)
    for (i in 1:(length(stringcheck))){
      check.i <- grepl("gender",stringcheck[i])
      if (check.i==TRUE){
        return(stringcheck[i])}
    }}}

ReturnMaxAge <- function(x){
  check <- grepl("maximumAge",x)
  if(check==TRUE){
    stringcheck <- str_split_fixed(x,",",Inf)
    for (i in 1:(length(stringcheck))){
      check.i <- grepl("maximumAge",stringcheck[i])
      if (check.i==TRUE){
        return(stringcheck[i])}
    }}}

ReturnMinAge <- function(x){
  check <- grepl("minimumAge",x)
  if(check==TRUE){
    stringcheck <- str_split_fixed(x,",",Inf)
    for (i in 1:(length(stringcheck))){
      check.i <- grepl("minimumAge",stringcheck[i])
      if (check.i==TRUE){
        return(stringcheck[i])}
    }}}

ReturnSort <-function(x){
  check <- grepl("sort",x)
  if(check==TRUE){
    stringcheck <- str_split_fixed(x,",",Inf)
    for (i in 1:(length(stringcheck))){
      check.i <- grepl("sort",stringcheck[i])
      if (check.i==TRUE){
        return(stringcheck[i])}
    }}}

ReturnLocation <- function(x){
  check <- grepl("currentLocation",x)
  if(check==TRUE){
    stringcheck <- str_split_fixed(x,",",Inf)
    for (i in 1:(length(stringcheck))){
      check.i <- grepl("currentLocation",stringcheck[i])
      if (check.i==TRUE){
        return(stringcheck[i])}
    }}}

CleanHash<- function(x){
  stringcheck <- str_split_fixed(x,"=",Inf)
  ifelse(length(stringcheck)>1,return(stringcheck[2]),"")
}

FlagNonSearch <- function(x){
  y <- grepl("search",x)
  z <- grepl("Categories",x)
  a <- grepl("distance",x)
  b <- grepl("gender",x)
  ifelse((y[1]==TRUE | z[1]==TRUE | a[1]==TRUE) | b[1]==TRUE,"",return("cut"))}