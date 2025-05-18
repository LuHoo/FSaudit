library(testthat)
library(FSaudit)

context("Attribute Binomial")

test_that("Attribute sample sizes Binomial match", {
  expect_equal(sizeBinom(.05, .1, 0), 29)
  expect_equal(sizeBinom(.05, .1, 1), 46)
  expect_equal(sizeBinomEer(.05, .1, .01)[[1]], 46)
})
test_that("Error message when parameters are not numeric", {
  expect_error(sizeBinom("bjkm", "25k", 3))
  expect_error(sizeBinom(.1, .01, TRUE))
  expect_error(sizeBinomEer("bjkm", "25k", 3)[[1]])
})
test_that("Error message when parameters alpha is <= 0 & >= 1", {
  expect_error(sizeBinom(-1, .1, 0))
  expect_error(sizeBinom(2, .1, 0))
  expect_error(sizeBinomEer(-1, .1, .01)[[1]])
  expect_error(sizeBinomEer(3, .1, .01)[[1]])
})
test_that("Error message when parameters tdr is <= 0 & >= 1", {
  expect_error(sizeBinom(0.05, 1, 0))
  expect_error(sizeBinom(0.05, -1, 0))
  expect_error(sizeBinomEer(0.05, 1, .01)[[1]])
  expect_error(sizeBinomEer(0.05, -1, .01)[[1]])
})
test_that("Error message when parameters c is not a positive integer", {
  expect_error(sizeBinom(.05, .05, -8))
  expect_error(sizeBinom(.05, .05, 2.1))
})
test_that("Error message when parameters eer is < 0 & >= 1", {
  expect_error(sizeBinomEer(0.05, .1, -1)[[1]])
  expect_error(sizeBinomEer(0.05, .1, 2)[[1]])
})
test_that("Error message when parameter eer > tdr", {
  expect_error(sizeBinomEer(.05, .05, .9)[[1]])
})


test_that("U.B. on the fraction of deviations for Binomial", {
  expect_equal(upperBinom(59, 0, .05), 0.04950761)
})
test_that("Error message when  parameter n is not a positive integer", {
  expect_error(upperBinom(-10, 5, .10))
  expect_error(upperBinom(10.5, 5, .10))
})
test_that("Error message when parameter alpha is <= 0 & >= 1", {
  expect_error(upperBinom(47, 5, -1))
  expect_error(upperBinom(47, 5, 2))
})
test_that("Error message when n < k", {
  expect_error(upperBinom(47, 50, .10))
})
test_that("Error message when k is not a positive integer", {
  expect_error(upperBinom(47, -1, .10))
  expect_error(upperBinom(47, 5.5, .10))
})


test_that("L.B. on fraction of deviations for sample with replacement", {
  expect_equal(lowerBinom(59, 6, .05), 0.04522264)
})
test_that("Error message when  parameter n is not a positive integer", {
  expect_error(lowerBinom(-10, 5, .10))
  expect_error(lowerBinom(10.7, 5, .10))
})
test_that("Error message when parameter alpha is <= 0 or >= 1", {
  expect_error(lowerBinom(47, 5, -1))
  expect_error(lowerBinom(47, 5, 2))
})
test_that("Error message when n < k", {
  expect_error(lowerBinom(47, 50, .10))
})
test_that("Error message when k is not a positive integer", {
  expect_error(lowerBinom(47, -1, .10))
  expect_error(lowerBinom(47, 2.75, .10))
})

test_that("Binomial lower bound matches", {
  alpha <- .025
  n <- 100
  k <- 1
  expect_equal(lowerBinom(n, k, alpha), 0.000253146)
})
