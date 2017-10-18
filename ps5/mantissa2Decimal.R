# Declaring the function: x string or binary number
Mantissa2Decimal <- function(x) {
  # Process the string and transform to decimal format
  sum(2^(-which(unlist(strsplit(as.character(x), "")) == 1)))
}