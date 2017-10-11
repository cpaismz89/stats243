## Auxiliary funcitons: swap and PIKKVF
# Swap function
swap <- function(x,i,j) {
  
  # Swap the values of x[i] and x[j]
  x[c(i,j)] <- x[c(j,i)]
  
  # Return the new vector
  return(x)
} 

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


# FYDV second version improved
FYKD2_V2 <- function(x, k, threshold = 800) {
  
  while (TRUE){
    # Generate k random numbers outside the loop, using the efficient PIKKVF 
    # function already implemented.
    j = PIKKVF(x, k, threshold)
    
    # List with indexes to exchange
    i = 1:k
    l1 = c(rbind(i, j))
    l2 = c(rbind(j, i))
    
    # Replace in place (vectorized swap version)
    x = swap(x, l1, l2)
    
    # If we have k different numbers (at least), break
    if (length(unique(x[1:k])) >= k){
      break
    }
  }  
  # Return relevant values
  return(x[1:k]) 
}


# FYDV third version improved
FYKD3_V3 <- function(x, k, threshold = 800) {
  
  while (TRUE){
    # Generate k random numbers outside the loop, using the efficient PIKKVF 
    # function already implemented.
    j = PIKKVF(x, k, threshold)
    
    # List with indexes to exchange
    i = 1:k
    l1 = c(rbind(i, j))
    l2 = c(rbind(j, i))
    
    # Replace in place (vectorized replace version)
    x = replace(x, l1, x[l2])
    
    # If we have k different numbers (at least), break
    if (length(unique(x[1:k])) >= k){
      break
    }
  }  
  # Return relevant values
  return(x[1:k]) 
}


