# Declaring the function: input vector of relevant numbers to compare
fullComparison <- function(nums){
  
  # Create a data frame with all relevant information
  ComparisonDF <- data.frame("decimal-2" = nums, 
                             "decimal-17" = format(nums, digits=17), 
                             hexadecimal = sprintf("%+13.13a", nums), 
                             "binary" = floatToBin(nums))
  
  # Change the name of the rows
  attributes(ComparisonDF)$row.names <- as.character(nums)
  attributes(ComparisonDF)$row.names[3] <- paste(nums[1], "+", nums[2])
  
  
  # Print the DF
  ComparisonDF
}