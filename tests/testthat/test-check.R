library(testthat)
library(FSaudit)

context("Check variable values")

a <- 1
b <- -1
d <- 0
vec <- c(a, b, d)

test_that("Positive value", {
  checkPositive(a) # No error message
  expect_error(checkPositive(b))
  expect_error(checkPositive(vec))
})

test_that("Nonzero value", {
  checkNonzero(a) # No error message
  expect_error(checkNonzero(d))
  expect_error(checkNonzero(vec))
})

test_that("Variable is greater than another", {
  checkLessthan(b, a) # No error message
  expect_error(checkLessthan(a, b))
})

test_that("Variable is in interval", {
  checkBetween(a, 0, 2) # No error message
  expect_error(checkBetween(a, 0, 1)) # a equal to right bound
  expect_error(checkBetween(a, 1, 2)) # a equal to left bound
  expect_error(checkBetween(a, 2, 3)) # a less than left bound
  expect_error(checkBetween(a, -1, 0)) # a greater than right bound
})

test_that("Variable is integer", {
  scientific <- 3.5e8
  char <- "How's that"
  vector <- c(1, 2)
  vecIntReal <- c(1, 1.1)
  factor <- factor(c("cat", "dog"))
  list <- list(a, char, vector, factor)
  checkInteger(a) # No error message
  checkInteger(vector) # No error message
  checkInteger(scientific) # No error message
  expect_error(checkInteger(a + .1))
  expect_error(checkInteger(char))
  expect_error(checkInteger(vecIntReal))
  expect_error(checkInteger(factor))
  expect_error(checkInteger(list))
})

test_that("Variable has valid option", {
  pets <- c("cat", "dog")
  checkOptions("dog", pets) # valid option
  expect_error(checkOptions("rabbit", pets)) # Invalid option
})

test_that("Variable is strictly positive or negative", {
  checkStrictsign(a) # No error message
  checkStrictsign(b) # No error message
  expect_error(checkStrictsign(vecPosNeg))
})

test_that("Vectors have same length", {
  checkLengths(a, b) # No error message
  expect_error(checkLengths(a, vec))
})

test_that("Vectors have numeric arguments", {
  checkNumeric(vec)
  expect_error(checkNumeric(pets))
})

test_that("Multiple items in unique ID are trapped", {
  inputID <- c(1, 2, 3, 4, 5, 5)
  expect_error(checkUnique(inputID))
})
