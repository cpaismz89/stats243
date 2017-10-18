# Declaring the function
CopyExplicit <- function(x){
  # Reference
  y <- x
  
  # Modification: new copy
  y[1] <- x[1]
  
  # Return the copy
  return(y)
}