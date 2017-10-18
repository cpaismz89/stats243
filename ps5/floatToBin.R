# Declaring the function
floatToBin <- function(x){
  # Select the integer part of the number
  intpart <- floor(x)
  
  # Decimal part of x
  decpart <- x - intpart
  
  # Binary representation of the integer part
  intbin <- R.utils::intToBin(intpart)
  
  # Binary representation of the fractional part
  decbin <- stringr::str_pad(R.utils::intToBin(decpart * 2^31), 31, pad="0")
  
  # Generate the number
  sub("[.]?0+$", "", paste0(intbin, ".", decbin)) 
}