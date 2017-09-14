# Declaring the function
CreateDataFrame <- function(html){
  # Turning off the warnings for visualization purposes
  options(warn = -1)
  
  # Libraries
  suppressMessages(library(XML))
  suppressMessages(library(RCurl))
  suppressMessages(library(stringr))
  
  # Check if the file is a parsed HTML
  if (typeof(html) != "externalptr"){
    stop("Not a valid html file from author's citation website
         please check the format of the input file")
  }
  # Get the links inside the html
  links <- getHTMLLinks(html, baseURL = urlInit, relative = FALSE)
  
  # Check if there are valid links in the file
  if (length(links) <= 0){
    stop("Not a valid html file from author's citation website
         the file does not contain any valid link")
  }
  
  # Generate the link containing the query
  url4queries <- paste("http://scholar.google.com", links[8], sep = '')
  
  # Global vectors for recording all the queries
  GTitles <- c()
  GJournals <- c()
  GAuthors <- c()
  GYears <- c()
  GCitations <- c()
  
  # Initial values: from (start) and size
  from <- 0
  size <- 100
  
  # Loop for getting all the articles based on the query structure
  while (TRUE){
    query <- paste("&cstart=",from,"&pagesize=",size,sep = '') 
    urlq <- paste(url4queries,query,sep = '')
    html <- readLines(urlq)
    html <- htmlParse(html)
    # Articles titles
    Titles <- xpathSApply(html, "//tbody[@id = 'gsc_a_b']//
                          tr[@class ='gsc_a_tr']//td[@class = 'gsc_a_t']
                          //a[@class = 'gsc_a_at']", xmlValue, 
                          trim = TRUE, encoding = "UTF-8") 
    
    # Break condition: no titles/papers inside the HTML object
    if (length(Titles) == 0 ) {
      break
    }
    
    # Journals and authors array
    JournalAuthors <- xpathSApply(html, "//tbody[@id = 'gsc_a_b']//
                                  tr[@class ='gsc_a_tr']//td[@class = 'gsc_a_t']
                                  //div[@class = 'gs_gray']", xmlValue, 
                                  trim = TRUE, encoding = "UTF-8")
    
    # Getting the Journals (even numbers of previos vector)
    Journals <- JournalAuthors[seq(2, length(JournalAuthors), 2)]
    
    # We delete the year of publication after the Journal
    Journals <- substring(Journals, 0, nchar(Journals)-6)
    
    # Getting the authors (odd numbers)
    Authors <- JournalAuthors[seq(1, length(JournalAuthors), 2)]
    
    # Years of publication
    Year <- xpathSApply(html, "//tbody[@id = 'gsc_a_b']//tr[@class ='gsc_a_tr']
                        //td[@class = 'gsc_a_y']//span[@class = 'gsc_a_h']", 
                        xmlValue, trim = TRUE, encoding = "UTF-8") 
    
    # Number of citations (No repetitions)
    Citations <- xpathSApply(html, "//tbody[@id = 'gsc_a_b']//
                             tr[@class ='gsc_a_tr']//td[@class = 'gsc_a_c']//a", 
                             xmlValue, trim = TRUE, encoding = "UTF-8")
    
    # Delete potential non numerical elements by transforming and filtering the array
    Citations <- as.numeric(Citations)
    remove <- c(NA)
    
    # Re-Transform it to a character array for the DF 
    Citations <- as.character(Citations[!Citations %in% remove])
    
    # Record in global vectors
    GTitles <- c(GTitles, Titles)
    GAuthors <- c(GAuthors, Authors)
    GJournals <- c(GJournals, Journals)
    GYears <- c(GYears, Year)
    GCitations <- c(GCitations, Citations)
    
    # Update the counter
    from <- from + 100
  }
  
  # Dimension checks: fill with 0 empty citations
  if (length(GCitations) != length(GTitles)){
    GCitations <- c(GCitations, integer(length(GTitles) - length(GCitations)))
  }
  
  # Create a DataFrame
  x_name <- "Title"
  y_name <- "Authors"
  z_name <- "Journals"
  w_name <- "Year"
  v_name <- "Citations"
  
  require(reshape2)
  ScholarDF <- melt(data.frame(GTitles, GAuthors, GJournals, GYears, GCitations))
  colnames(ScholarDF) <- c(x_name, y_name, z_name, w_name, v_name)
  
  # Return the DataFrame
  return(ScholarDF)
}