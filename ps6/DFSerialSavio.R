library(readr)
library(chron)
library(dplyr)
library(microbenchmark)

setwd("/global/scratch/paciorek/wikistats_full/dated_for_R/")
system('bash -c ls > ~/ps6/P4/filelist.txt')

files <- readLines('~/ps6/P4/filelist.txt')
nfiles <- length(files)

## Version 1: create filtered files
SerialloopF <- function(files, ListDF, aux){
  for(i in files){
    cmd <- paste("bash -c \"grep \"Barack_Obama\" ", i, " > ~/ps6/P4/" , i, "Fil", 
                 "\"", sep = "")
    system(cmd, intern = TRUE)
    filteredFile <-paste("~/ps6/P4/", i, "Fil", sep = "")
    DF <- readr::read_delim(file=filteredFile, delim = " ", quote ="\"")
    
    # Postprocessing
    # Date and time as characters
    names(DF) <- c('date','time','lang','concept', 'hits', 'Y')
    DF$date <- as.character(DF$date)
    DF$time <- as.character(DF$time)
    
    # Time format
    DF$time[DF$time %in%  c("0", "1")] <- "000000"
    wh <- which(nchar(DF$time) == 5)
    DF$time[wh] <- paste0("0", DF$time[wh])
    
    
    # Date format
    DF$date <- as.Date(DF$date, "%Y%m%d") 
    
    # List of DFs
    ListDF[[aux]] <- DF
    aux <- aux + 1
  }
  
  # Remove DF
  remove(DF)
  
  # Total DataFrame
  TDF <- bind_rows(ListDF[1:aux - 1])
  TDF$date <- as.character(TDF$date)
  TDF$time <- as.character(TDF$time)
  
  # Chron object: Date and time format
  TDF$chron <- chron(TDF$date, TDF$time,
                     format = c(dates = 'y-m-d', times = "hms"))
  
  # Time zone format for Date and Hour
  TDF$chron <- as.POSIXlt(TDF$chron, tz="EST")
  
  # Order by date (for simplicity)
  TDF <- TDF[order(TDF$chron),]
  
  # Return full DataFrame
  return(TDF)
}

# Initial Data
ListDF <- list(seq(1, nfiles, 1))
aux <- 1

# Time benchmark
print(microbenchmark(Results <- SerialloopF(files, ListDF, aux), times = 1))

