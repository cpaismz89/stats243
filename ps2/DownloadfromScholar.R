# ---- DownloadfromScholar-ext ----
# Declaring the function
DownloadfromScholar <- function(name) {
  # Setting working directory
  setwd("~/ps2/")
  
  # Loading libraries
  library(XML)
  library(stringr)
  
  # Turning off the warnings for visualization purposes
  options(warn = -1)
  
  # Check if the input is a string
  if (!is.character(name)){
    stop("The input must be a name (e.g Geoffrey Hinton)")
  }
  
  # Check if we have only letters and a first and last name
  if(grepl("^[[:alpha:]]+[[:space:]]+[[:alpha:]]+$", name) == FALSE){
    stop("Only two words (no digits) are allowed as the author's name,
         please try again (e.g Geoffrey Hinton)")
  }
  
  # Check if the name string has a first and last name
  if (length(strsplit(name, " ")[[1]]) < 2){
    stop("The name string must have a space between the first and last names,
         please try again (e.g Geoffrey Hinton)")
  }
  
  # Separate the name in first and last names
  firstname <- strsplit(name, " ")[[1]][[1]]
  lastname <- strsplit(name, " ")[[1]][[2]]
  
  # Generate the initial URL to download the .html file
  urlInit <- paste("https://scholar.google.com/scholar?q=", firstname,
                   "+", lastname, "&hl=en&as_sdt=0,5", sep = "")
  filename <- paste(firstname, lastname, ".html", sep = "")
  
  # Download the file and parse the html file
  download.file(urlInit, filename)
  parsedHTML <- htmlParse(readLines(filename))
  
  # Find the link to the author's personal website
  link <- xpathSApply(parsedHTML, "//h4[@class = 'gs_rt2']//a", 
                      xmlGetAttr, "href")
  
  # Check if the link exists (is valid)
  if (length(link) <= 0) {
    stop("New error: The author does not exist in the Google Scholar Database,
         please try again with a different name")
  }
  
  # Extract the user
  UserID <- str_match(link, ".*user=([\\d\\w]+)&")[[2]]
  
  # If no User ID, then author does not exists in Google Scholar: stop
  if (is.na(UserID) == TRUE){
    stop("The author does not exist in the Google Scholar Database,
         please try again with a different name")
  }
  
  # Get the citations html: generate the url and download it
  urlcites <- paste("https://scholar.google.com", link, sep = "")
  filenamecites <- paste(UserID, ".html", sep = "")
  download.file(urlcites, filenamecites)
  
  # Parse the new html
  parhtmlcites <- htmlParse(readLines(filenamecites))
  
  # Return parsed html file
  return(parhtmlcites)
}  