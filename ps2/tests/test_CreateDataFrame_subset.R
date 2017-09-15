# ---- testCreateDataFramesubset-ext ----
source("CreateDataFrame_subset.R")
setwd("C:/Users/chile/ps2/RTests/")

# Testing the CreateDataFrame_subset function (input = HTML, output = Dataframe)
test_that("Types and lengths are consistent when using a known html file", {
  # Save the output in the FinalDF variable, reading a html file from Google Scholar
  # As an example, we are using the citation website of Albert Einstein
  FinalDF <- CreateDataFrame_subset(htmlParse(readLines("qc6CJjYAAAAJ.html")))
  
  # Check if the final output is a DataFrame
  expect_that( FinalDF, is_a("data.frame") )
  
  # Check if all columns have the same length
  expect_that( length(FinalDF$Title), equals(length(FinalDF$Authors)) )
  expect_that( length(FinalDF$Title), equals(length(FinalDF$Year)) )
  expect_that( length(FinalDF$Title), equals(length(FinalDF$Citations)) )
  expect_that( length(FinalDF$Title), equals(length(FinalDF$Journals)) )
  
  # Check if the Citations column has only numbers
  expect_that( as.numeric(FinalDF$Citations), is_a("numeric") )
})

# Testing the CreateDataFrame_subset function for different inputs
test_that("Output when a non-existent user html file is provided", {
  # Save the output in the FinalDF variable, after reading a html 
  # file from Google Scholar website, but from a non-existing user
  FinalDF2 <- CreateDataFrame_subset(htmlParse(readLines("nonexistent.html")))
})

# Another html file (not from Google Scholar)
test_that("A html file/code from any page", {
  # Html taken from google.com
  FinalDF <- CreateDataFrame_subset(htmlParse(readLines("index.html")))
})

# Numbers as inputs
test_that("Erroneous input is provided (numbers)", {
  # Testing with a numeric input
  FinalDF2 <- CreateDataFrame_subset(1222)
})

# String with numbers
test_that("Erroneous input is provided (digit strings)", {
  # String composed by digits
  FinalDF2 <- CreateDataFrame_subset("1222")
})

# Any random string
test_that("Erroneous input is provided (character strings)", {
  # Characters string
  FinalDF2 <- CreateDataFrame_subset("hello")
})

# Some html sintax (check if function is confused)
test_that("Erroneous input is provided (html sintax)", {
  # Html sintaxis
  FinalDF <- CreateDataFrame_subset("<html><body>")
})