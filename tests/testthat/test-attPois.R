library(testthat)
library(FSaudit)

context("Attribute Poisson")

test_that("Attribute sample sizes poisson match", {
  expect_equal(sizePois(.05, .1, 0), 30)
  expect_equal(sizePois(.05, .1, 1), 48)
  expect_equal(sizePoisEer(.05, .1, .01)[[1]], 48)
})
test_that("Error message when parameters are not numeric", {
  expect_error(sizePois("bjkm", "25k", 3))
  expect_error(sizePois(.1, .01, TRUE))
  expect_error(sizePoisEer("bjkm", "25k", 3)[[1]])
})
test_that("Error message when parameters alpha is <= 0 & >= 1", {
  expect_error(sizePois(-1, .1, 0))
  expect_error(sizePois(2, .1, 0))
  expect_error(sizePoisEer(-1, .1, .01)[[1]])
  expect_error(sizePoisEer(3, .1, .01)[[1]])
})
test_that("Error message when parameters tdr is <= 0 & >= 1", {
  expect_error(sizePois(0.05, 1, 0))
  expect_error(sizePois(0.05, -1, 0))
  expect_error(sizePoisEer(0.05, 1, .01)[[1]])
  expect_error(sizePoisEer(0.05, -1, .01)[[1]])
})
test_that("Error message when parameters c is not a positive integer", {
  expect_error(sizePois(.05, .05, -8))
  expect_error(sizePois(.05, .05, 2.1))
})
test_that("Error message when parameters eer is < 0 & >= 1", {
  expect_error(sizePoisEer(0.05, .1, -1)[[1]])
  expect_error(sizePoisEer(0.05, .1, 2)[[1]])
})
test_that("Error message when parameter eer > tdr", {
  expect_error(sizePoisEer(.05, .05, .9)[[1]])
})


test_that("U.B. on the fraction of deviations for poisson", {
  expect_equal(upperPois(59, 0, .05), 0.05077512)
})
test_that("Error message when  parameter n is not a positive integer", {
  expect_error(upperPois(-10, 5, .10))
  expect_error(upperPois(10.5, 5, .10))
})
test_that("Error message when parameter alpha is <= 0 & >= 1", {
  expect_error(upperPois(47, 5, -1))
  expect_error(upperPois(47, 5, 2))
})
test_that("Error message when n < k", {
  expect_error(upperPois(47, 50, .10))
})
test_that("Error message when k is not a positive integer", {
  expect_error(upperPois(47, -1, .10))
  expect_error(upperPois(47, 5.5, .10))
})

test_that("Lower bound Poisson matches DAfA3", {
  n <- 100
  expect_equal(lowerPois(n, 1, .2), 0.223 / n, tolerance = 1e-5)
  expect_equal(lowerPois(n, 2, .2), 0.824 / n, tolerance = 1e-5)
  expect_equal(lowerPois(n, 3, .2), 1.535 / n, tolerance = 1e-5)
  expect_equal(lowerPois(n, 4, .2), 2.296 / n, tolerance = 1e-5)
  expect_equal(lowerPois(n, 5, .2), 3.089 / n, tolerance = 1e-5)
  expect_equal(lowerPois(n, 1, .15), 0.162 / n, tolerance = 1e-5)
  expect_equal(lowerPois(n, 1, .1), 0.105 / n, tolerance = 1e-5)
  expect_equal(lowerPois(n, 1, .05), 0.051 / n, tolerance = 1e-5)
  expect_equal(lowerPois(n, 1, .025), 0.025 / n, tolerance = 1e-5)
  expect_equal(lowerPois(n, 1, .01), 0.01 / n, tolerance = 1e-5)
  expect_equal(lowerPois(n, 1, .005), 0.005 / n, tolerance = 1e-5)
  expect_equal(lowerPois(n, 1, .001), 0.001 / n, tolerance = 1e-5)
})

test_that("Upper bound Poisson matches DAfA3", {
  n <- 100
  expect_equal(upperPois(n, 1, .2), 2.995 / n, tolerance = 1e-5)
  expect_equal(upperPois(n, 2, .2), 4.280 / n, tolerance = 1e-5)
  expect_equal(upperPois(n, 3, .2), 5.516 / n, tolerance = 1e-5)
  expect_equal(upperPois(n, 4, .2), 6.721 / n, tolerance = 1e-5)
  expect_equal(upperPois(n, 5, .2), 7.906 / n, tolerance = 1e-5)
  expect_equal(upperPois(n, 1, .15), 3.373 / n, tolerance = 1e-5)
  expect_equal(upperPois(n, 1, .1), 3.890 / n, tolerance = 1e-5)
  expect_equal(upperPois(n, 1, .05), 4.744 / n, tolerance = 1e-5)
  expect_equal(upperPois(n, 1, .025), 5.572 / n, tolerance = 1e-5)
  expect_equal(upperPois(n, 1, .01), 6.639 / n, tolerance = 1e-5)
  expect_equal(upperPois(n, 1, .005), 7.431 / n, tolerance = 1e-5)
  expect_equal(upperPois(n, 1, .001), 9.234 / n, tolerance = 1e-5)
})
