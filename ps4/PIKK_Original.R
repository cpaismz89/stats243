# Original PIKK function
PIKK <- function(x, k) { 
  x[sort(runif(length(x)), index.return = TRUE)$ix[1:k]] 
}