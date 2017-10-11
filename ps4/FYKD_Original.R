# Original function
FYKD <- function(x, k) {
  n <- length(x) 
  for(i in 1:n) { 
    j = sample(i:n, 1) 
    tmp <- x[i] 
    x[i] <- x[j] 
    x[j] <- tmp 
  } 
  return(x[1:k]) 
}
