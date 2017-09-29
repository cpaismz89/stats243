# ---- ListSubs-ext ----
# Declaring the function
ListSubs <- function(ORList, ORPattern, RPattern){
  
  # Loading libraries
  library("stringr")
  
  # Loading functions
  source("PatternSubstitution.R")
  
  # Apply the Substitution function to a list
  NewList <- lapply(ORList, function(x) PatternSubstitution(x, ORPattern, RPattern))
  
  # Return the new list
  return(NewList)
}

# ---- Listgsub-ext ----
# Declaring the function
Listgsub <- function(ORList, ORPattern, RPattern){
  
  # Loading libraries
  library("stringr")
  
  # Apply the Substitution function to a list
  NewList <- lapply(ORList, function(x) gsub(ORPattern, RPattern, x))
  
  # Return the new list
  return(NewList)
}


# ---- ListExt-ext ----
# Declaring the function
ListExt <- function(ORList, ToExtIndexes){
  
  # Loading libraries
  library("stringr")
  
  # Loading functions
  source("ExtractUnlist.R")
  
  # Apply the Extract function to a list
  ExtList <- lapply(seq_along(ORList), function(x) ExtractUnlist(ORList, x, ToExtIndexes))
  
  # Return the extracted data (as a list)
  return(ExtList)
}

# ---- ListLines-ext ----
# Declaring the function
ListLines <- function(ORList, Pattern){
  
  # Loading libraries
  library("stringr")
  
   # Apply the Extract function to a list
  LinesList <- lapply(ORList, function(x) grep(Pattern, unlist(x)))
  
  # Return the extracted data (as a list)
  return(LinesList)
}

# ---- ListDrop-ext ----
# Declaring the function
ListDrop <- function(ORList, ToDropIndexes){
  
  # Loading libraries
  library("stringr")
  
  # Loading functions
  source("DropUnlist.R")
  
  # Apply the Drop function to a list
  NewList <- lapply(seq_along(ORList), function(x) DropUnlist(ORList, x, ToDropIndexes))
  
  # Return the new list)
  return(NewList)
}