#
# Misc. helpers for the shiny app
#

############################################################################################
# Access Index Helpers
############################################################################################

calculate_aggregated_index <- function(transport_mode, types, cost) {
  if (transport_mode=="drive") {
    df <- driving_index
  } else {
    df <- transit_index
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
  return(df[apply(as.data.frame(df[,col])==1,1,any),])
}