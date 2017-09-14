# Declaring the function
CreateDataFrame_subset <- function(html){
  # Turning off the warnings for visualization purposes
  options(warn = -1)
  
  # Libraries
  suppressMessages(library(XML))
  suppressMessages(library(RCurl))
  suppressMessages(library(stringr))
  
  # Check if the file is a parsed HTML
  if (typeof(html) != "externalptr"){
    stop("Not a valid html file from author's citation website")
  }
  
  # Articles titles
  Titles <- xpathSApply(html, "//tbody[@id = 'gsc_a_b']//tr[@class ='gsc_a_tr']
                        //td[@class = 'gsc_a_t']//a[@class = 'gsc_a_at']", 
                        xmlValue, trim = TRUE, encoding = "UTF-8") 
  
  # Check if there are valid titles: if not, not a valid file
  if (length(Titles) <= 0){
    stop("No Titles: There is no author's citation page inside Google Scholar
         associated with the html file being processed, please
         try again with a different file")
  }
  
  # Journals and authors array
  JournalAuthors <- xpathSApply(html, "//tbody[@id = 'gsc_a_b']//
                                tr[@class ='gsc_a_tr']//td[@class = 'gsc_a_t']
                                //div[@class = 'gs_gray']", xmlValue, 
                                trim = TRUE, encoding = "UTF-8")
  
  # Getting the Journals (even numbers of previos vector)
  Journals <- JournalAuthors[seq(2, length(JournalAuthors), 2)]
  
  # We delete the year of publication after the Journal
  Journals <- substring(Journals, 0, nchar(Journals) - 6)
  
  # Getting the authors (odd numbers)
  Authors <- JournalAuthors[seq(1, length(JournalAuthors), 2)]
  
  # Years of publication
  Year <- xpathSApply(html, "//tbody[@id = 'gsc_a_b']//tr[@class ='gsc_a_tr']
                      //td[@class = 'gsc_a_y']//span[@class = 'gsc_a_h']", 
                      xmlValue, trim = TRUE, encoding = "UTF-8") 
  
  # Number of citations (including repeated articles)
  Citations <- xpathSApply(html, "//tbody[@id = 'gsc_a_b']
                           //tr[@class ='gsc_a_tr']//td[@class = 'gsc_a_c']
                           //a", xmlValue, trim = TRUE, encoding = "UTF-8")
  
  # Delete potential non numerical elements by transforming and filtering the array
  Citations <- as.numeric(Citations)
  remove <- c(NA)
  
  # Re-Transform it to a character array for the DF 
  Citations <- as.character(Citations[!Citations %in% remove])
  
  # Dimension checks: fill with 0 empty citations
  if (length(Citations) != length(Titles)){
    Citations <- c(Citations, integer(length(Titles) - length(Citations)))
  }
  # Create a DataFrame
  x_name <- "Title"
  y_name <- "Authors"
  z_name <- "Journals"
  w_name <- "Year"
  v_name <- "Citations"
  
  # Create the DF using the reshape2 melt function for simplicity
  suppressMessages(require(reshape2))
  ScholarDF <- suppressMessages(melt(data.frame(Titles, Authors, Journals, Year, Citations)))
  
  # Define the columns' names
  suppressMessages(colnames(ScholarDF) <- c(x_name, y_name, z_name, w_name, v_name))
  
  # Return the DataFrame
  return(ScholarDF)
}