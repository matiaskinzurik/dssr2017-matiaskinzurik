# Importing testthat library, so that we can use the unit testing functions
library("testthat")

# Importing all functions from my script under test
source("C:/users/matia/Desktop/Program/BioscreenProgram.R")

test_that("File is a CSV", {
  # input
  input = "C:/users/matia/Desktop/Program/Bioscreen_Raw.csv"
  
  # expected_output is row means of the data
  exp_output = is_a("data.frame")
  
  # real_output
  real_output = Open_File(input)
  
  # compare
  expect_that(real_output, exp_output)
})

test_that("File is not a CSV", {
  input = "C:/users/matia/Desktop/Program/ReadMe.txt"
  #exp_output = 
  #real_output = Open_File(input)
  # compare
  expect_that(Open_File(input), throws_error("File is not a CSV"))
})

# Test sigmoid function

# Test PlotAll function

test_that("Everything plotted correctly", {
  input = 
})