# Declaring the function: input x = number 
FloatingFormat <- function(x){
  # Set working directory
  setwd("C:/Users/Lenovo/Desktop/UCBerkeley/3er Semestre/STATS 243/HW/HW5/")
  
  # Loading libraries and functions
  library("pryr")
  source("Binary2Decimal.R")
  
  # Calling the bits() function
  x <- bits(x)
  x <- gsub(" ", "", x)
  
  # Extract Elements
  S <- gsub(" ", "", substr(x, 1, 1))
  e <- gsub(" ", "", substr(x, 2, 12))
  d <- gsub(" ", "", substr(x, 13, nchar(x)))
  
  # Prints out the different elements of the number representation
  print(paste("Sign (S):", S, "- Length:", nchar(S), "- Number:",  
              Binary2Decimal(S), sep = " "))
  print(paste("Exponent (e):", e, "- Length:", nchar(e), "- Number:",    
              Binary2Decimal(e), sep = " "))
  print(paste("Mantissa (d):", d, "- Length:", nchar(d), sep = " "))
}

