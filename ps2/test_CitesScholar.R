# ---- testCitesScholar-ext ----
source("CitesScholar.R")

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