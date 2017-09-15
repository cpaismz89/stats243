# ---- testCitesScholar-ext ----
source("CitesScholar.R")

# Testing the CitesScholar function (input = HTML, output = list)
test_that("Types and lengths are consistent when using a real author's name", {
  # As an example, we are using the citation website of the well known Albert Einstein
  ResultsList <- CitesScholar("Albert Einstein")
  
  # Check if the final output is a list
  expect_that( ResultsList, is_a("list") )
  
  # Check its length
  expect_that( length(ResultsList), equals(2) )
  
  # Check the types of both outputs
  expect_that( ResultsList[[1]], is_a("character") )
  expect_that( ResultsList[[2]], is_a("HTMLInternalDocument") )
})

# Testing the CitesScholar function (input = HTML, output = list)
test_that("Output when a non-existent author name is provided", {
  # A non-registered user name is provided
  ResultsList <- CitesScholar("Cristobal Pais")
})

# Testing the CitesScholar function for different inputs
test_that("Only first name", {
  # One word is provided
  ResultsList <- CitesScholar("Albert")
})

test_that("More than two names", {
  # More than 2 words are provided
  ResultsList <- CitesScholar("Albert Einstein Canalejo")
})

test_that("Words and numbers", {
  # First and last name with a number
  ResultsList <- CitesScholar("Albert Einstein2")
})

test_that("Words and numbers (mixed)", {
  # Numbers in between characters
  ResultsList <- CitesScholar("Albert E1nstein")
})

test_that("No space between names", {
  # First and last names are not separated
  ResultsList <- CitesScholar("AlbertEinstein")
})

test_that("Uppercase name", {
  # Name is written in uppercase
  ResultsList <- CitesScholar("ALBERT EINSTEIN")
})

test_that("Lowercase name", {
  # Name is written in lowercase
  ResultsList <- CitesScholar("albert einstein")
})

test_that("Erroneous input is provided (numbers)", {
  # Numeric input
  ResultsList <- CitesScholar(1123)
})

test_that("Erroneous input is provided (digit strings)", {
  # String with digits
  FinalDF2 <- CitesScholar("1222")
})

test_that("Erroneous input is provided (digit strings)", {
  # String with digits
  FinalDF2 <- CitesScholar("1222")
})

test_that("Erroneous input is provided (anything)", {
  # Html sintaxis
  FinalDF <- CitesScholar("<html><body>")
})