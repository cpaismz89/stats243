# Setting the working directory
setwd("C:/Users/chile/Desktop/Stats243/HW/HW3/Code/Working/Functions/26Sep/")

# Loading libraries
library("stringr")
library("reshape2")
library("ggplot2")

# Loading functions
source("plotSummary.R")

## Creating Data Frame
# Columns are defined
Columns <- list(Years, Titles, NACTS, NScenes, NWords, NUniqueSpeakers, 
                NSpoken, NUniqueWords, AverageWordsChunk)

# Format is applied to each columns
ProcCols <- lapply(Columns, function(x) str_split(paste(x, collapse = '---'), "---")) 

# DataFrame is created and columns names are assigned
suppressMessages(SummaryDF <- melt(data.frame(ProcCols)))
colnames(SummaryDF) <- c("YEAR", "TITLE", "NACTS", "NSCENES", "NWORDS", "NSPEAKERS","NCHUNKS",
                         "NUNIQUE_WORDS","AVG_WORDS")

# Save the DataFrame as txt file
write.table(SummaryDF,"P2EDataFrame.txt",sep = ",")

# Numeric transformation
SummaryDF$YEAR <- as.numeric(as.character(SummaryDF$YEAR))
SummaryDF$NSCENES <- as.numeric(as.character(SummaryDF$NSCENES))
SummaryDF$NACTS <- as.numeric(as.character(SummaryDF$NACTS))
SummaryDF$NWORDS <- as.numeric(as.character(SummaryDF$NWORDS))
SummaryDF$NSPEAKERS <- as.numeric(as.character(SummaryDF$NSPEAKERS))
SummaryDF$NCHUNKS <- as.numeric(as.character(SummaryDF$NCHUNKS))
SummaryDF$NUNIQUE_WORDS <- as.numeric(as.character(SummaryDF$NUNIQUE_WORDS))
SummaryDF$AVG_WORDS <- as.numeric(as.character(SummaryDF$AVG_WORDS))

# Plots using the plotSummary function
plotSummary(SummaryDF$YEAR, SummaryDF$NSCENES, SummaryDF, y = "Number of Scenes", color = "red")
plotSummary(SummaryDF$YEAR, SummaryDF$ACTS, SummaryDF, y = "Number of ACTs", color = "blue")
plotSummary(SummaryDF$YEAR, SummaryDF$NWORDS, SummaryDF, y = "Number of Words", color = "#40b8d0")
plotSummary(SummaryDF$YEAR, SummaryDF$NSPEAKERS, SummaryDF, y = "Number of Speakers", color = "#00b33c")
plotSummary(SummaryDF$YEAR, SummaryDF$NCHUNKS, SummaryDF, y = "Number of Chunks", color ="#e64d00")
plotSummary(SummaryDF$YEAR, SummaryDF$NUNIQUE_WORDS, SummaryDF, y = "Number of Unique Words", color ="#751aff")
plotSummary(SummaryDF$YEAR, SummaryDF$AVG_WORDS, SummaryDF, y = "AVG Number of words", color = "#ff6666")