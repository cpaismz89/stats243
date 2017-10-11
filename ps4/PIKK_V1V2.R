# First new function: PIKKV1
PIKKV1 <- function(x, k){
  # Initialization of selected numbers array and size variable
  sel = integer(k)
  size = k
  
  # Variable for keeping track of the current not visited entries of vector x 
  left = length(x)
  
  # For loop: select elements based on k_{i}/n_{i} probability, ending up with the 
  # desire number of entries k
  aux = 1
  for (i in 1:length(x)){
    
    # Probability of selecting an element
    if (runif(1) <= size / left){
      size = size - 1
      sel[aux] <- x[i]
      aux = aux + 1
    }
    
    # Break if all k entries have been selected
    if (size == 0){
      break
    }
    
    # Update the remaining number of entries
    left = left - 1
  }
  
  # Return selected values (k)
  return(sel)
}


# PIKK Version 2 function
PIKKV2 <- function(x, k){
  # While loop: True until condition is satisfied
  while(TRUE){
    
    # Indexes array (unique elements) is generated
    indexes <- unique(ceiling(runif(k, 0, length(x))))
    
    # If size is k: stop, otherwise repeat procedure
    if (length(indexes) == k) {
      break
    }
  }
  
  # Return relevant (k) entries
  return(x[indexes])
}