# Declaring the function: x string or binary number
Binary2Decimal <- function(x) {
  # Process the string and transform to decimal format
  sum(2^(which(rev(unlist(strsplit(as.character(x), "")) == 1))-1))
}