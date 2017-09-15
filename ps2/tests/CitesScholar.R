# ---- CitesScholar-ext ----
# Declaring the function
CitesScholar <- function(name) {
  # Turning off the warnings for visualization purposes
  options(warn = -1)
  
  # Loading libraries
  suppressMessages(library(XML))
  suppressMessages(library(RCurl))
  suppressMessages(library(stringr))
  
  # Check if the input is a string
  if (!is.character(name)){
    stop("The input must be a name (e.g Geoffrey Hinton)")
  }
  
  # Check if we have only letters and a first and last name
  if (grepl("^[[:alpha:]]+[[:space:]]+[[:alpha:]]+$", name) == FALSE){
    stop(paste("Only two words (no digits) are allowed as the author's name.",
               "The name string must have a space between the first and last names",
               "please try again (e.g Geoffrey Hinton)", sep = " "))
  }
 
  # Separate the name in first and last names
  firstname <- strsplit(name, " ")[[1]][[1]]
  lastname <- strsplit(name, " ")[[1]][[2]]
  
  # Generate the URL with the profile in google scholar
  urlInit <- paste("https://scholar.google.com/scholar?hl=es&q=", firstname,
                   "+", lastname, "&btnG=&lr=", sep = "")
  
  # Get the Google Scholar ID and the link to the personal citations website
  rawHTML <- readLines(urlInit)
  
  # Parse the raw HTML code
  parsedHTML <- htmlParse(rawHTML)
  
  # Find the link to the author's personal website
  link <- xpathSApply(parsedHTML, "//h4[@class = 'gs_rt2']//a", 
                      xmlGetAttr, "href")
  
  # Check if there are valid links inside the file: if not, stop
  if (length(link) <= 0) {
    stop(paste("The author does not exist in the Google Scholar Database,",
               "please try again with a different name", sep = " "))
  }
  
  # Extract the user
  UserID <- str_match(link, ".*user=([\\d\\w]+)&")[[2]]
  
  # If no User ID, then author does not exists in Google Scholar: stop
  if (is.na(UserID) == TRUE){
    stop(paste("The author does not exist in the Google Scholar Database,",
               "please try again with a different name", sep = " "))
  }
  
  # Get the citations html
  urlcites <- paste("https://scholar.google.com", link, sep = "")
  htmlcites <- readLines(urlcites)
  parhtmlcites <- htmlParse(htmlcites)
  
  # Return the user ID and parsed html from the personal citations web
  returnlist <- list(UserID, parhtmlcites)
  return(returnlist)
}