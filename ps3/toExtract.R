# ---- toExtract-ext ----
# Declaring the function
toExtract <- function(rawShakespeare, num){
  # Loading libraries
  library("stringr")
  
  # Pre-Process Extraction and cleaning Debug
  DirPattern <- paste('^[\r\n]\\s{6,}([^\r\n&\\d<#\\[]*[\r\n]){1,',
                      as.character(num), '}\\s[\r\n]', sep = '')
  ToExtract <- str_extract_all(preprocShakespeareColl, 
                               regex(pattern = DirPattern, multiline = TRUE))
  
  # Pattern for filtering all the relevant scene directions based on a series
  # of keywords.
  KeywordsPattern <- paste('[\\b\\.\\s]*(([Ee]nters?)|(Exit)|(Here,?)|(Epitaph)|',
                           '(pass(es)?\\s)|([aA]larum)|([Tt]rumpets?)|(Sound)|',
                           '(sings\\s[^b])|(Falstaff)|(The\\stwo)|(throws)|([Ee]xeunt',
                           '[^\\.])|(Re\\-[eE]nter)|(fights)|([Ff]lourish)|',
                           '(come\\sforward)|([Gg]host)|(opens)|(Retreat)|',
                           '(After)|(cry)|(speaks)|(approaches)|(goes)|',
                           '(Apparition)|(comments)|(FAIRIES)|(appears)|',
                           '(ORATION)|(Grace)|(stand))[\\b\\.\\s]*', sep = '')
  
  # Extraction of relevant scene directions without taking out SONGS
  FilteredToExtract <- ToExtract[[1]][grep(KeywordsPattern, ToExtract[[1]])]
  FilteredToExtract <- unique(FilteredToExtract)
  length(FilteredToExtract)
  
  # Scene directions are deleted from the text
  for (n in 1:length(FilteredToExtract)) {
    preprocShakespeareColl <- gsub(x = preprocShakespeareColl, 
                                   pattern = FilteredToExtract[n], 
                                   replacement = paste("[", 
                                                       FilteredToExtract[n],
                                                       "]",
                                                       sep = ''),
                                   perl = TRUE)
  }
  
  # Return the processed object
  return(preprocShakespeareColl)
  
}