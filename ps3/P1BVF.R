# Setting the working directory
setwd("C:/Users/chile/Desktop/Stats243/HW/HW3/Code/Working/Functions/26Sep/")

# Loading libraries
library("stringr")

# Loading functions
source("ExtractandSubst.R")
source("PrintUtils.R")

# Read the Plays file and check its length
PlaysShakespeare <- readLines("ShakespearePlays.txt") 
length(PlaysShakespeare)

## Extraction Process
# Years: Extraction pattern definition, extraction and recorded as a vector
YearPattern <- '^\\d{4}\\s+'
Years <- unlist(lapply(PlaysShakespeare, 
                       function(x) ExtractandSubs(x, YearPattern, ' ', ''))
) 

# Plays' Titles extraction
TitlePattern <- '([A-Z\',;]+\\s+){1,7}\\s+--by'
Titles <- unlist(lapply(PlaysShakespeare, 
                        function(x) ExtractandSubs(x, TitlePattern, '\\s--by', ''))
)

# Number of Acts extraction
ACTSPattern <- '&&[^&]*&&'
NACTS <- unlist(lapply(PlaysShakespeare, 
                       function(x) length(unique(unlist(ExtractandSubs(x, ACTSPattern)))))
)

# Number of Scenes extration
SPattern <- '#[^#]*#'
NScenes <- unlist(lapply(PlaysShakespeare,
                         function(x) length(unlist(ExtractandSubs(x, SPattern))))
)

# Play Body extraction
BodyPattern <- '\\&&[Aa][Cc][Tt]\\s+\\w+&&\\s+#[S][Cc][Ee][Nn][Ee]\\s[1Ii]#.*'
BodyPlay <- unlist(lapply(PlaysShakespeare, 
                          function(x) ExtractandSubs(x, BodyPattern))
)


## Outputs: txt files for each relevant field
Lists <- list(Years, Titles, NACTS, NScenes, BodyPlay)
FileNames <- c("P1BYears.txt", "P1BTitles.txt", "P1BNACTS.txt",
               "P1BNScenes.txt","P1BBody.txt")
List2Txt(Lists, FileNames)

# Create DataFrame and save it as a txt/csv file
Cols <- list(Years, Titles, NACTS, NScenes)
ColNames <- c("Years", "Titles", "NActs", "NScenes")
DF2Txt(Cols, ColNames, "P1BDataFrame.txt")
