# ---- PrintUtils-ext ----
# Declaring the function
List2Txt <- function(Lists, filenames, OPTcollapse = '\n'){
  
  # Write the list as a collapsed column separating elements by a new line (default)
  for (n in 1:length(Lists)){
    write(paste(unlist(Lists[n]), collapse = OPTcollapse), filenames[n])    
  }
}


# Declaring the function
DF2Txt <- function(ColumnsArray, ColumnNames, filename, OPTSep = ','){
  
  # Loading libraries
  library("stringr")
  library("reshape2")
  
  # Split the columns by entries
  for (n in 1:length(ColumnsArray)){
    ColumnsArray[n] <- str_split(paste(unlist(ColumnsArray[n]),collapse = '---'),
                                 pattern = "---")
  }
  
  # Create the DF
  suppressMessages(DF <- melt(data.frame(ColumnsArray)))
  colnames(DF) <- ColumnNames
  
  # Save DF as a csv file (default)
  write.table(DF, filename ,sep = OPTSep)
}