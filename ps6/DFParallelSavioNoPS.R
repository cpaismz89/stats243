require(parallel) 
require(doParallel)
library(foreach)

library(readr)
library(chron)
library(dplyr)
library(microbenchmark)

setwd("/global/scratch/paciorek/wikistats_full/dated_for_R/")
system('bash -c ls > ~/ps6/P4/filelist.txt')

mcoptions <- list(preschedule=FALSE)
nCores <- makeCluster(24)
registerDoParallel(nCores)

files <- readLines('~/ps6/P4/filelist.txt')
nfiles <- length(files)

## Version 1.2: create filtered files parallel
ParallelloopF <- function(files){
  
  # Create a Full DF with foreach combining rows  
  foreach(i = files,
          .options.multicore=mcoptions, 
          .combine = rbind) %dopar% {
            
            # Bash command for filtering files
            cmd <- paste("bash -c \"grep \"Barack_Obama\" ", i, " > ~/ps6/P4/" , i, "Fil", 
                         "\"", sep = "")
            system(cmd, intern = TRUE)
            
            # Read filtered file
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
            
            # Return DF
            DF
          }
}


## Run the functions
# Initial Data
#files <- c("part-00000", "part-00001")

# Time benchmark and release cores
print(microbenchmark(TDF <- ParallelloopF(files), times = 1))
stopCluster(nCores)

# Post-Processing
TDF$date <- as.character(TDF$date)
TDF$time <- as.character(TDF$time)

# Chron object: Date and time format
TDF$chron <- chron(TDF$date, TDF$time,
                   format = c(dates = 'y-m-d', times = "hms"))

# Time zone format for Date and Hour
TDF$chron <- as.POSIXlt(TDF$chron, tz="EST")

# Order by date (for simplicity)
TDF <- TDF[order(TDF$chron),]