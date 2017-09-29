# ---- DropUnlist-ext ----
# Declaring the function
DropUnlist <- function(Data, index, todropData){
  
  # Drop elements from a nested list based on indexes inside another list
  # if there are matching elements
  if (length(unlist(todropData[index])) > 0){
    NewData <- unlist(Data[index])[-unlist(todropData[index])]
  }  
  
  # Otherwise, return the original Data
  else{
    NewData <- unlist(Data[index])
  }
  
  # Return new list without dropped data 
  return(NewData)
}