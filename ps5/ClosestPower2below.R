# Declaring the function
ClosestPower2below <- function(x){
  # Variable to return initialization
  exponent <- 0
  
  # Loop for finding the value 
  while (TRUE){
    if (2^(exponent+1) > x){
      break
    }
    else{
      exponent <- exponent + 1
    }
  }
  
  # Return exponent value
  return(exponent)
}