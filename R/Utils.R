# Function for initial estimate of incidence from change in cumulative, then replace negative changes with NAs
change_rmv_nas <- function(vec){
  # Find values that result in negative changes and replace with NA
  diff_vec <- c(NA, diff(vec))
  vec2 <- vec
  vec2[is.na(vec)] <- NA
  vec2[diff_vec < 0] <- NA
  
  return(vec2)
}


# Function to return difference of cumsum vectors removing NAs
cumsum_diffs <- function(vec1, vec2, vec3){
  vec1[is.na(vec1)] <- 0
  vec2[is.na(vec2)] <- 0
  vec3[is.na(vec3)] <- 0
  
  out <- cumsum(vec1) - cumsum(vec2) - cumsum(vec3)
  
  return(out)
}
