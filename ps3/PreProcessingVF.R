# ---- PreProcessing-ext ----
# Declaring the function
PreProcessing <- function(rawShakespeare){
  
  ## Calling external functions
  # Pattern substitution function
  source("PatternSubstitution.R")
  
  ## Pre-processing
  # Remove header, footer and sonnets based on lines
  rawShakespeare <- rawShakespeare[-(1:2815)]
  rawShakespeare <- rawShakespeare[-(121163:length(rawShakespeare))]
  
  # Remove extra text
  rawShakespeare <- paste(rawShakespeare, collapse = '\r\n')
  rawShakespeare <- gsub('<<[^>]*>>', '', rawShakespeare)
  rawShakespeare <- str_split(rawShakespeare, '\r\n')[[1]]
  
  
  ## General Patterns and replacements: Add/Delete characters for easier processing
  # ACTs
  ACTPattern <- '^([Aa][Cc][Tt][_]?\\s?[a-zA-Z0-9]{1,3})\\.?'
  ACTReplace <- "&&\\1\\&&"
  
  # Scenes
  SCPattern <- '(\\b[S][Cc][Ee][Nn][Ee]\\b\\s+\\w+)\\.?'
  SCReplace <- '#\\1#'
  
  # Epilogues and Prologues
  EPPattern <- '(.*EPILOGUE)'
  EPReplace <- '%%\\1'
  
  PRPattern0 <- '^\\s{2}PROLOGUE\\.'
  PRReplace0 <- '<>QUINCE~'
  
  PRPattern <- '(.*PROLOGUE)'
  PRReplace <- '[\\1]'
  
  # Characters: <> at the beginning for each character
  CHPattern1 <- '^\\s{1,2}([A-Z\',\\-]{2,}\\.(\\s+(&\\s)?[A-Z\\-]{2,}){1,})\\.(\\s{0,2}[a-zA-Z\'\\[])'
  CHReplace1 <- '<>\\1~ \\4'
  
  CHPattern2 <- '^\\s{1,2}([A-Z\',\\-]{2,}(\\s+(&\\s)?[a-zA-Z\'&,\\-]+){0,3})\\.(\\s*[a-zA-Z\'\\[])'
  CHReplace2 <- '<>\\1~ \\4'
  
  CHPattern3 <- '^\\s{2}([GW][A-Z]+)(\\s{2}\\[)'
  CHReplace3 <- '<>\\1~\\2'
  
  # Characters: lowercase characters special case (Cap. Wife. is reformated)
  CHLCPattern0 <- '^\\s{2}([F]l[a-z]+)(.*)'
  CHLCReplace0 <- '\\[\\1\\2\\]'  
  
  CHLCPattern1 <- '^(\\s{2}Cap)\\.(\\sWife\\.)'
  CHLCReplace1 <- '\\1\\2'  
  
  CHLCPattern2 <- '^\\s{2}([A-Z][a-z]+(\\s{1}[A-Z][a-z]+){0,})\\.(\\s{0,3}[iA-Z\'\\[\\(])'  
  CHLCReplace2 <- '<>\\1~ \\3'
  
  CHLCPattern3 <- '^\\s{4}((King)|(Queen)|(Luc))\\.(\\s((\'T)|([A-Z])))'
  CHLCReplace3 <- '<>\\1~ \\5'
  
  CHLCPattern4 <- '^\\s{2}(Fool)\\.(\\s{6}H)'
  CHLCReplace4 <- '<>\\1~ \\2'
  
  # Characters: Starting with number and lowercase cases
  CHNPattern <- '^\\s{2}(\\d+\\.\\s[A-Za-z]+)\\.(.*)'
  CHNReplace <- '<>\\1~\\2'
  
  # Characters: Commedy of Errors special formats
  CHCEPattern1 <- '^([A-Z\',\\-]{2,}(\\s+(&\\s)?[A-Z\'\\-]+){0,2})\\.(\\s{1,2}[oA-Z\'\\[]\\s?[\',OHT!FSVLWa-z\\-])'
  CHCEReplace1 <- '<>\\1~\\4'
  
  CHCEPattern2 <- '^\\s{4}([A-Z\',\\-]{2,}(\\s+(&\\s)?[A-Z\'\\-]+){0,2})\\.(\\s{4}[oA-Z\'\\[])'
  CHCEReplace2 <- '<>\\1~\\4'
  
  # Extra processing: Speakers with 3 to 5 spaces at the beginning
  EXPattern1 <- '^\\s{3,5}([A-Z]{2,}(\\s[A-Z]+){0,})\\.(\\s[A-Z\'])'
  EXReplace1 <- '<>\\1~ \\3'
  
  # Extra processing: Special BOTH and MALVOLIO speakers with extra spaces after dot
  EXPattern2 <- '^\\s{2}([A-Z]{2,})\\.(\\s+[A-Z\'\\[])'
  EXReplace2 <- '<>\\1~ \\2'
  
  # Songs
  SONGPattern1 <- '(([a-zA-Z\']+\\s)?S[Oo][Nn][Gg]\\.?)$'
  SONGReplace1 <- '\\[\\1\\]'
  
  SONGPattern2 <- '(\\s+(WINTER|SPRING))$'
  SONGReplace2 <- '\\[\\2\\]'
  
  # Special Song: Autolycus singing
  SSONGPattern <- '\\[Enter\\sAUTOLYCUS,\\ssinging\\]'
  SSONGReplace <- '<>AUTOLYCUS~'
  
  # Special Epilogue: Prospero
  SEPPattern <- '\\s+Spoken\\sby\\s(PROSPERO)'
  SEPReplace <- '<>\\1~'
  
  # Special Prologue
  SPRPattern <- '\\s{8,}(TROILUS\\sAND\\sCRESSIDA)'
  SPRReplace <- '<>\\1~'
  
  # Titles
  TIPattern <- '^(by.*)'
  TIReplace <- '--\\1--'
  
  # Special instructions (scenes/stage directions)
  SPPattern1 <- '\\(((within)|([Ss]ings)|([Rr]eads)|(rises))([^\\)]*)\\)'
  SPReplace1 <- '\\[\\1\\]'
  
  # Patterns and replacements list
  PatRepList <- list(
    c(ACTPattern, ACTReplace), c(SCPattern, SCReplace),
    c(EPPattern, EPReplace), c(PRPattern0, PRReplace0),
    c(CHPattern1, CHReplace1), c(CHPattern2, CHReplace2), 
    c(CHPattern3, CHReplace3), c(PRPattern, PRReplace),
    c(CHLCPattern0, CHLCReplace0), c(CHLCPattern1, CHLCReplace1), 
    c(CHLCPattern2, CHLCReplace2), c(CHLCPattern3, CHLCReplace3), 
    c(CHLCPattern4, CHLCReplace4), c(CHNPattern, CHNReplace),
    c(CHCEPattern1, CHCEReplace1), c(CHCEPattern2, CHCEReplace2), 
    c(EXPattern1, EXReplace1), c(EXPattern2, EXReplace2),
    c(SONGPattern1, SONGReplace1), c(SONGPattern2, SONGReplace2),
    c(SSONGPattern, SSONGReplace), c(TIPattern, TIReplace),
    c(SEPPattern,SEPReplace), c(SPPattern1, SPReplace1),
    c(SPRPattern, SPRReplace)
  )
  
  # Calling Substitution function for all patterns
  for (n in 1:length(PatRepList)){
    Patt <- PatRepList[[n]][1]
    Repl <- PatRepList[[n]][2]
    rawShakespeare <- PatternSubstitution(rawShakespeare, Patt, Repl)
  }
  
  # Return pre-preprocessed Shakespeare
  return(rawShakespeare)
}



# ---- PreProcessing2-ext ----
# Declaring the function
PreProcessing2 <- function(rawShakespeare){
  
  ## Calling external functions
  # Pattern substitution function
  source("PatternSubstitution.R")
  
  # Split by new line  
  rawShakespeare <- str_split(rawShakespeare, '\r\n')[[1]]
  
  ## General Patterns and replacements: Add/Delete characters for easier processing
  # Stage Info: Stage Directions and Scene Information [ at the beginning and ] at the end
  STGPattern1 <- '\\s*([<]?[^\\[](Exit)|(Enter)|(Re-enter)|(Exeunt)|(Flourish)|(Here\\sa\\s)|(A\\slong\\sflourish,)\\.?)(.*)'
  STGReplace1 <- '\\[\\1\\9\\]'
  
  STGPattern2 <- '^(\\s{20,}[a-z]+)(.*)'
  STGReplace2 <- '\\[\\1\\2\\]'
  
  STGPattern3 <- '^\\s{6,}([A-Z]{2,},)(.*)'
  STGReplace3 <- '\\[\\1\\2\\]'
  
  # Stage Info: Comedy of errors special format
  STCEPattern1 <- '^(((Exit)|(Enter)|(Re-enter)|(Exeunt)|(Flourish))\\.?)(.*)'
  STCEReplace1 <- '\\[\\1\\8\\]'
  
  STCEPattern2 <- '^(Exit|Enter|Re-enter|Exeunt)(.*)'
  STCEReplace2 <- '\\[\\1\\2\\]'
  
  # Special instructions (scenes/stage directions)
  SPPattern1 <- '([A-Za-z\';:,!]\\s?\\.?)\\s{5,}(.*)\\.$'
  SPReplace1 <- '\\1         [\\2\\]'
  
  SPPattern2 <- '([A-Za-z\';:,!]\\s?\\.?)\\s{5,}([^\\[]+\\.)(\\[.*\\])$'
  SPReplace2 <- '\\1 [\\2\\] \\3'
  
  SPPattern3 <- '^\\s{18,}([A-Z]{2,}[^\\.\':]*)(?<!\\.,\';:)$'
  SPReplace3 <- '\\[\\1\\]'
  
  SPPattern4 <- '^\\s{30,}([^:#\\[]*(?<!([\',!;:])|([A-Z]\\.))$)'
  SPReplace4 <- '\\[\\1\\]'
  
  SPPattern5 <- '(THE\\sORDER.*)'
  SPReplace5 <- '\\[\\1\\]'
  
  SPPattern6 <- '^([a-z].*)'
  SPReplace6 <- '\\[\\1\\]'
  
  SPPattern7 <- '\\(((within)|([Ss]ings)|([Rr]eads)|(rises))([^\\)]*)\\)'
  SPReplace7 <- '\\[\\1\\]'
  
  SPPattern8 <- '^\\s{13}([A-Z]{2,})(.*)'
  SPReplace8 <- '\\[\\1\\2\\]'
  
  SPPattern9 <- '^\\s{8,}([Dd]ance\\.)'
  SPReplace9 <- '\\[\\1\\]'
  
  SPPattern10 <- '^\\s*([Tt]he\\s+[Tt]rumpet\\s+sounds?\\.)'
  SPReplace10 <- '\\[\\1\\]'
  
  SPPattern11 <- '([A-Z]+)\\)'
  SPReplace11 <- '\\1\\]'
  
  SPPattern12 <- '(They\\sfight\\.)(.*)'
  SPReplace12 <- '\\[\\1\\2\\]'
  
  SPPattern13 <- '(Here\\sthey\\se)(.*)'
  SPReplace13 <- '\\[\\1\\2\\]'
  
  # Patterns and replacements list
  PatRepList <- list(
    c(STGPattern1, STGReplace1), c(STGPattern2, STGReplace2), 
    c(STGPattern3, STGReplace3), c(STCEPattern1, STCEReplace1), 
    #c(STCEPattern2, STCEReplace2),
    c(SPPattern1, SPReplace1), c(SPPattern2, SPReplace2), 
    c(SPPattern3, SPReplace3), c(SPPattern4, SPReplace4), 
    c(SPPattern5, SPReplace5), c(SPPattern6, SPReplace6),
    c(SPPattern8, SPReplace8), c(SPPattern9, SPReplace9),
    c(SPPattern10, SPReplace10), c(SPPattern11, SPReplace11),
    c(SPPattern12, SPReplace12), c(SPPattern13, SPReplace13)
  )
  
  # Calling Substitution function for all patterns
  for (n in 1:length(PatRepList)){
    Patt <- PatRepList[[n]][1]
    Repl <- PatRepList[[n]][2]
    rawShakespeare <- PatternSubstitution(rawShakespeare, Patt, Repl)
  }
  
  # Return pre-preprocessed Shakespeare
  return(rawShakespeare)
}