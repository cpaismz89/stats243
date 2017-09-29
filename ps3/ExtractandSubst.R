# ---- ExtractandSubst-ext ----
# Declaring the function
ExtractandSubs <- function(string, extractPatt, toreplacePat = NULL, subsPatt = NULL){
  
  # Loading libraries
  library("stringr")
  
  # Main operation: if subs patterns, call gsub. Otherwise, only str_extract
  if (!is.null(toreplacePat) & !is.null(subsPatt)){
    string <- gsub(toreplacePat, subsPatt, str_extract_all(string, extractPatt))  
  }
  else {
    string <- str_extract_all(string, extractPatt)  
  }
  
  # Return new string
  return(string)
}