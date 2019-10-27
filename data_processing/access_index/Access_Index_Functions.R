######
######
#Access Index Functions

#base function
fxn <- function(vector, scale){
  #2^(-vector/scale)
  ifelse(vector<=scale,1,(1/(vector/scale))^2)
}

#for use in subseting

Make_Subset <- function(dataframe,type=NULL,cost=NULL){
  dataframe$n <- 1
  if(is.null(type)){
    dataframe <- dataframe
  }
  if(length(type)==1){
    dataframe <- dataframe[dataframe[,type],]
  }
  if(length(type)>1){
    dataframe <- dataframe[apply(FUN=any,X=dataframe[,c(type)],MARGIN=1),]
  }
  if(is.null(cost)){
    return(dataframe)
  }
  else{
    dataframe <- dataframe <- dataframe[dataframe[,cost],]
  }
}

#3. Then we aggregate sessions the subset per the specifications

Aggregate_Subset <- function(dataframe){
  return(aggregate(n ~ lat_lon, data=dataframe,FUN=sum)) #note that "n" in this case ==1 per row, which n this case corresponds to one unique program session
}

#4. Here we merge the aggregated sessiosn into the block_distance data set to incorporate travel times 

Merge_Set <- function(dataframe){
  relevant_columns <- c("Id2","lat_lon","driving_morning","transit_morning")
                        #,"kilometers","block_lat","block_lon") #had originally required these, but I beileve they are relics
  block_distance_new <- block_distance[,relevant_columns] #here we're cutting duplicate program_addresses, and other antiquated info (previous aggregations, PCT vhcl ownership) that was use in previous calculations
  temp <- merge(block_distance_new,dataframe,all.y=TRUE)
  temp$n[is.na(temp$n)==TRUE] <- 0
  return(temp)
}

#5 Here we define the gravity threshold decay function. Note that as of July 25th we've decided not use this equation, and instead use the exponential equation per the results of the sensitivity analysis. 

decay_fxn <- function(dataframe,mode){
  if (mode=="transit") {
    scale = 5
    column="transit_morning"
  }
  if (mode=="drive"){
    scale = 5
    column="driving_morning"
  }
  mode=mode
  dataframe[,"n"]*(ifelse(dataframe[,column]<=scale,1,(1/(dataframe[,column]/scale))^2))
}

#6 Here we define the expoential decay function

decay_fxn_exp <- function(dataframe,mode){
  if (mode=="transit") {
    scale = 5
    column="transit_morning"
  }
  if (mode=="drive"){
    scale = 5
    column="driving_morning"
  }
  mode=mode
  dataframe[,"n"]*exp(-dataframe[,column]/scale)
}

decay_fxn_softGravity <- function(dataframe,mode){
  if (mode=="transit") {
    scale = 10
    column="transit_morning"
  }
  if (mode=="drive"){
    scale = 10
    column="driving_morning"
  }
  mode=mode
  dataframe[,"n"]*(1/(1+(dataframe[,column]/scale))^2)
}

#7 here we define a function to normalize results on a 0-1 scale. Note that after we calculate an total acess index in this script, max will be == the max AI of any one blockgroup.

normalize <- function(vec,max) {
  return((vec/max)*100)
}