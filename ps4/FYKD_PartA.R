## FYKDV Version 1 function
FYKDV1 <- function(x, k) {
  
  # Length of the vector
  n <- length(x) 
  
  # Loop up to k instead of n
  for(i in 1:k) { 
    j = sample(i:n, 1) 
    tmp <- x[i] 
    x[i] <- x[j] 
    x[j] <- tmp 
  } 
  
  # Return the relevant values
  return(x[1:k]) 
}


## FYKD Version 2 function
# Auxiliary swap function
swap <- function(x, i, j) {
  
  # Swap the values of x[i] and x[j]
  x[c(i, j)] <- x[c(j, i)]
  
  # Return the new vector
  return(x)
} 

# FYKD second version
FYKDV2 <- function(x, k) {
 
  # Generate k random numbers outside the loop, following the same rules as in
  # the original function.
  j = runif(k, 1:k, length(x))
  
  # Swap in place loop 
  for(i in 1:k) { 
    if(i != j[i]) {
      x = swap(x, i, j[i]) 
      }
  }
  
  # Return relevant values
  return(x[1:k]) 
}


# FYKD Version 3 function
FYKDV3 <- function(x, k) {
  
  # Generate k random numbers outside the loop, following the same rules as in
  # the original function.
  j = runif(k, 1:k, length(x))
  
  # Replace in place loop 
  for(i in 1:k) { 
    if (i != j[i]){
      x = replace(x, c(i, j[i]), x[c(j[i], i)])
    }
  }
  
  # Return relevant values
  return(x[1:k]) 
}