# PIKK VF function 
PIKKVF <- function(y, k, threshold = 800, tune = 0){
  # Check if extra terms are added for increasing the probability of success
  ifelse(length(y)/k <= threshold, tune <- k/6, tune <- 0)
  
  # Indexes loop  
  while (TRUE){
    indexes = unique(ceiling(runif(k + tune, 0, length(y)) ))
    if (length(indexes) >= k){
      return(y[indexes[1:k]])
    }
  }
}

# PIKK VF function with extra scale adjustement
PIKKVF_scale <- function(y, k, threshold = 800, tune = 0, scale = 6){
  # Check if extra terms are added for increasing the probability of success
  ifelse(length(y)/k <= threshold, tune <- k/scale, tune <- 0)
  
  # Indexes loop  
  while (TRUE){
    indexes = unique(ceiling(runif(k + tune, 0, length(y)) ))
    if (length(indexes) >= k){
      return(y[indexes[1:k]])
    }
  }
}