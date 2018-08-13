#
# Misc. helpers for the shiny app
#

############################################################################################
# Access Index Helpers
############################################################################################

get_race_access_means <- function(access_inds){
  race_col_names <- c("Hispanc","White","Black")
  race_names <- c("Hispanic", "White", "Black")
  race_access_list <- list()
  for(i in 1:length(race_col_names)){
    race_pops <- shape_census_block@data[,race_col_names[i]]
    tot_race_pop <- sum(shape_census_block@data[,race_col_names[i]])
    race_access_list[race_names[i]] <- sum(access_inds*(race_pops/tot_race_pop))
  }
  return(race_access_list)
}

calculate_aggregated_index <- function(transport_mode, types, cost, disability, block=TRUE) {
  if (transport_mode=="drive") {
    if (disability) {
      if (block) {df <- driving_index_disability}
      else {df <- driving_index_disability_nbhd}
      }
    else {
      if (block) {df <- driving_index}
      else {df <- driving_index_nbhd}
      }
  } else {
    if (disability) {
      if (block) {df <- transit_index_disability}
      else {df <- transit_index_disability_nbhd}
      }
    else {
      if (block) {df <- transit_index}
      else {df <- transit_index_nbhd}
      }
  }
  # look for the intersection of indices containing the words in the string-vector "types"
  # and the string "cost"
  shared_indices <- stack(sapply(FUN=grep,X=c(types,cost),x=colnames(df)))$values
  shared_indices <- shared_indices[duplicated(shared_indices)]
  
  if (length(shared_indices)>1) {
    val <- rowMeans(df[,shared_indices])
  } else {
    val <- df[,shared_indices]
  }
  return(val)
}

############################################################################################
# Subsetting the data
############################################################################################

# Function to subset all the resource datasets based on the neighborhood selected
subset_for_neighborhoods <- function(df, neighborhoods_list){
  if ("All neighborhoods" %in% neighborhoods_list) {
    a <- df
  }
  else {
    a <- df[which(df[, "nbhd_name"] %in% neighborhoods_list),]
  }
  return(a) 
}

# Subsetting the data for cost
subset_for_cost <- function(df, min_cost, max_cost) {
  return(df[df$session_cost >= min_cost & 
              df$session_cost  <= max_cost,])
}

# Subsetting the data for the type of the program selected
subset_for_category <- function(df, col) {
  #if(is.null(df) || nrow(df)==0) {
  if(nrow(df)==0 || is.null(col)) {
    return(df[0,])
  }
  return(df[apply(as.data.frame(df[,col])==1,1,any),])
}

# Subsetting for programs that allow special needs
subset_for_special_needs <- function(df) {
  return(df[df[,"has_special_needs_offerings"],])
}