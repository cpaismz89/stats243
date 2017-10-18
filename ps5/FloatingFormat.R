# Declaring the function: input x = number 
FloatingFormat <- function(x){
  # Set working directory
  setwd("C:/Users/Lenovo/Desktop/UCBerkeley/3er Semestre/STATS 243/HW/HW5/")

  # Loading libraries and functions
  library("pryr")
  source("Binary2Decimal.R")
  source("Mantissa2Decimal.R")
  
  # Calling the bits() function
  y <- bits(x)
  y <- gsub(" ", "", y)
  
  # Extract Elements
  S <- gsub(" ", "", substr(y, 1, 1))
  e <- gsub(" ", "", substr(y, 2, 12))
  d <- gsub(" ", "", substr(y, 13, nchar(y)))
  
  # Prints out the different elements of the number representation
  print(paste("Sign (S):", S, "- Length:", nchar(S), "- Number:",  
              Binary2Decimal(S), sep = " "))
  print(paste("Exponent (e):", e, "- Length:", nchar(e), "- Number:",    
              Binary2Decimal(e), sep = " "))
  print(paste("Mantissa (d):", d, "- Length:", nchar(d), sep = " "))
  print(paste("              Number: ", Mantissa2Decimal(d), sep = " "))
  print(paste("Number representation: ", x, " = (-1)^", 
              Binary2Decimal(S), " * (", 
              1 + Mantissa2Decimal(d), ") * 2^(", 
              Binary2Decimal(e), " - 1023)", 
              sep = ""))
}
