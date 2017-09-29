# Setting the working directory
setwd("C:/Users/chile/Desktop/Stats243/HW/HW3/Code/Working/Functions/26Sep/")

# Loading libraries
library("stringr")

# Loading functions
source("PreProcessingVF.R")
source("toExtract.R")
source("PatternSubstitution.R")

# Check if the Shakespeare file is in the current directory
# If it does not exist: Download the file from given url
filename = "pg100.txt"
if (!file.exists(filename)) {
  url = "http://www.gutenberg.org/cache/epub/100/pg100.txt"
  download.file(url, filename)
}

# Read the file
rawShakespeare <- readLines(filename)

# Firts pre-process Step: Explicit scene directions
# Pre-Process the text using the PreProcessing function
preprocShakespeare <- PreProcessing(rawShakespeare)

# Debug: Print the current state of the file
write(preprocShakespeare, "DEBUG_P1A.txt")

# Collapse the text into one long line
preprocShakespeareColl <- as.character(paste(preprocShakespeare, collapse = '\r\n'))

# Main cleaning loop: iterates over the length of the scene direction chunk
# without taking out the Songs inside the text
for (n in 2:15){
  preprocShakespeareColl <- toExtract(preprocShakespeareColl, n)  
}

# Second pre-process step: More scene directions
# Pre-Process the text using the PreProcessing2 function
postprocShakespeare <- PreProcessing2(preprocShakespeareColl)
postprocShakespeare <- paste(postprocShakespeare, collapse = '\r\n')

# Save the current state of the file (Debugging purposes)
write(unlist(str_split(postprocShakespeare, '\r\n')) , "DEBUG_P1A.txt")

# Separate plays by endings: Delete newlines and split by endings obtaining 
# a character vector 
postprocShakespeareColl <- gsub('(\r\n)|(\r)|(\n)', ' ', postprocShakespeare)
PlaysShakespeare <- strsplit(postprocShakespeareColl, 
                             '\\s+[-]?THE END[-]?\\s+')[[1]]

# Checking the total number of plays inside the file
length(PlaysShakespeare)

# Save information for visualization purposes
write(PlaysShakespeare, "ShakespearePlays.txt")