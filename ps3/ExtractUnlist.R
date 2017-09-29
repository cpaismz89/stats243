# ---- ExtractUnlist-ext ----
# Declaring the function
ExtractUnlist <- function(Data, index, todropData){
  
  # Extract elements from a nested list based on indexes inside another list
  ExtractedData <- unlist(Data[index])[unlist(todropData[index])]
  
  # Return extracted Data 
  return(ExtractedData)
}