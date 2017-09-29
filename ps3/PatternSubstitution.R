# ---- PatternSubstitution-ext ----
# Declaring the function
PatternSubstitution <- function(string, pattern, replace){
  
  # Looks for the pattern, replaces it and returns the new string
  string <- gsub(pattern, replace, string, perl = TRUE)
  return(string)
}