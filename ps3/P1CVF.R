# Setting the working directory
setwd("C:/Users/chile/Desktop/Stats243/HW/HW3/Code/Working/Functions/26Sep/")

# Loading libraries
library("stringr")

# Loading functions
source("PatternSubstitution.R")
source("DropUnlist.R")
source("ExtractUnlist.R")
source("ListSubs.R")

# Read the Body file and check its length
PlaysBody <- readLines("P1BBody.txt") 
length(PlaysBody)

# Full cleaning []: Patterns are declared and concatenated 
CPattern <- '\\[[^#\\[\\]>]*(\\[[^#>\\]\\[]*)?\\]([^#>\\[\\]]*\\])?'
CPattern2 <- '\\[[^<>\\]]*'
CPattern3 <- '\\s+(\\[)|(\\])\\s*'
CPatterns <- c(CPattern, CPattern2, CPattern3)

# Cleaning loop over all "cleaning" patterns
for (n in 1:length(CPatterns)){
  PlaysBody <- ListSubs(PlaysBody, CPatterns[n], '')
}

# Extract dialog chunks after taking out the ACT headers AND Epilogues
ACTPattern <- '&&A'
ACTReplace <- '<>A'
EPIPattern <- '%%'
EPIReplace <- '<>'
SCPattern <- '(\\s*)#(S[Cc][Ee][Nn][Ee])'
SCReplace <- '\\1<>\\2'

AESPatterns <- list(c(ACTPattern, ACTReplace), c(EPIPattern, EPIReplace),
                    c(SCPattern, SCReplace))

# Replacing loop over all patterns
for (n in 1:length(CPatterns)){
  PlaysBody <- ListSubs(PlaysBody, AESPatterns[[n]][1], AESPatterns[[n]][2])
}

# Split the Body by spoken chunks 
PlaysBody <- lapply(PlaysBody, function(x) str_split(x, '<>'))

## Dropping Acts/Scenes, Epilogue and empty strings
# Detect lines with ACTs and Scenes for dropping them
LNACTPatern <- '^([aA][cC][tT])|(S[Cc][Ee][Nn][Ee])'
ACTDrop <- ListLines(PlaysBody, LNACTPatern) 

# Extract Acts/Scenes lines (visualization purposes)
InfoACTScenesDrop <- ListExt(PlaysBody, ACTDrop)  

# Drop Acts/Scenes lines from the Body
PlaysBody <- ListDrop(PlaysBody, ACTDrop)   

# Detect lines with Epilogues for dropping them
EPDPattern <- '.*EPILOGUE'
EpilogueDrop <- ListLines(PlaysBody, EPDPattern) 

# Extract Epilogue lines
InfoEpilogues <- ListExt(PlaysBody, EpilogueDrop)   

# Drop Epilogue lines
PlaysBody <-  ListDrop(PlaysBody, EpilogueDrop) 

# Empty Space
PlaysBody <- lapply(seq_along(PlaysBody), 
                    function(x) {
                      unlist(PlaysBody[x])[unlist(PlaysBody[x]) != ""]
                    }
                    )

# Final cleaning step: Treatment by line for special scene directions                    
# Special Patterns are declared
SPPat1 <- '\\s+(Sound\\sa\\sflourish.*)'
SPPat2 <- '\\s+(He\\stakes\\sup.*)'
SPPat3 <- '\\s+(The\\sLORDS\\srise.*)'
SPPat4 <- '\\s+(During\\sthis\\ssong.*)'
SPPat5 <- '\\s+(\\d\\.\\sA\\slively\\sflourish.*)'
SPPat6 <- '\\s+(THE\\sVISION\\..*)'
SPPat7 <- '\\s+Then\\s+enter([^,]*,){3}' 
SPPat8 <- '\\s+TITUS\\s+kills\\s+him.*'
SPPat9 <- '\\s+The\\sfoot\\sabove.*'
SPPat10 <- '\\s+Ross,\\sand\\sW.*'
SPPat11 <- '\\s+before\\shim;[^A-Z]*S.*'
SPPat12 <- '\\s+The\\sdrum\\sp.*'
SPPat13 <- 'CARDINAL\\sB.*'
SPPat14 <- 'LA\\sPUCELLE\\sand\\sYORK\\sfight.*'
SPPat15 <- '\\s{10,15}SIR\\s+[A-Z].*'
SPPat16 <- 'The\\sCAPTAINS.*'
SPPat17 <- 'OF\\sTHE\\sP.*'
SPPat18 <- '\\sEPHESUS,.*'
SPPat19 <- '\\s+DROMIO\\sOF\\sSYRACUSE\\sas.*'
SPPat20 <- '\\s{2,}enter\\s[A-Z].*'
SPPat21 <- '\\s+Here,\\safter.*'
SPPat22 <- '\\s+The\\sbattle\\sc.*'
SPPat23 <- '\\s+Iago\\sfrom.*'
SPPat24 <- 'CYMBELINE,.*'

FCPatterns <- c(SPPat1, SPPat2, SPPat3, SPPat4, SPPat5, SPPat6, SPPat7, SPPat8, 
                SPPat9, SPPat10, SPPat11, SPPat12, SPPat13, SPPat14, SPPat15, 
                SPPat16, SPPat17, SPPat18, SPPat19, SPPat20, SPPat21, SPPat22,
                SPPat23, SPPat24)

# New List for cleaning: original format is kept as PlaysBody list
CleanPlays <- PlaysBody

# Cleaning loop
for(n in 1:length(FCPatterns)){
  CleanPlays <- Listgsub(CleanPlays, FCPatterns[n], '')
}

# Separate each spoken chunk and speaker: nested lists for a perfect match 
# between speaker and spoken chunk
CleanPlaysSep <- lapply(seq_along(CleanPlays), 
                     function(x) (str_split(unlist(CleanPlays[x]), "~")))

# Output Files
# Save the clean body file as a txt 
write(unlist(CleanPlays), "CleanPlaysBody.txt")

# Save the speakers/chunk separated clean body as a txt 
write(unlist(CleanPlaysSep), "SpeakersChunks.txt")

# Output Examples
CleanPlaysSep[[1]][1:2]
CleanPlaysSep[[15]][2:4]
CleanPlaysSep[[36]][10:13]