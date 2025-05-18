library(testthat)
library(FSaudit)

context("Attribute Sampling")

test_that("U.B. on the number of deviations for the sample", {
  expect_equal(upper(5, 1000000, 59, .05, dist = "pois"), 178188)
  expect_equal(upper(5, 1000000, 59, .05, dist = "binom"), 169963)
  expect_equal(upper(5, 1000000, 59, .05),  169960)
})
test_that("U.B. on the number of deviations poisson match", {
  expect_equal(upper(5, 1000000, 59, .05, dist = "pois"),
               ceiling(upperPois(59, 5, .05) * 1000000))
})
test_that("U.B. on the number of deviations binomial match", {
  expect_equal(upper(5, 1000000, 59, .05, dist = "binom"),
               ceiling(upperBinom(59, 5, .05) * 1000000))
})
test_that("U.B. on the number of deviations hypergeometric match", {
  expect_equal(upper(5, 1000000, 59, .05),
               upperHyper(1000000, 59, 5, .05))
})

test_that("L.B. on the number of deviations for the sample", {
  expect_equal(lower(5, 1000000, 59, .05, dist = "pois"), 33392)
  expect_equal(lower(5, 1000000, 59, .05, dist = "binom"), 33987)
  expect_equal(lower(5, 1000000, 59, .05),  33989)
})

test_that("L.B. on the number of deviations poisson match", {
  expect_equal(lower(5, 1000000, 59, .05, dist = "pois"),
               floor(lowerPois(59, 5, .05) * 1000000))
})

test_that("L.B. on the number of deviations binomial match", {
  expect_equal(lower(5, 1000000, 59, .05, dist = "binom"),
               floor(lowerBinom(59, 5, .05) * 1000000))
})

test_that("L.B. on the number of deviations hypergeometric match", {
  expect_equal(lower(5, 1000000, 59, .05),
               lowerHyper(1000000, 59, 5, .05))
})

test_that("L.B. on the number of deviations poisson match", {
  expect_equal(lower(k = 5, n = 59, alpha = .05, dist = "pois"),
               lowerPois(59, 5, .05))
})

test_that("L.B. on the number of deviations binomial match", {
  expect_equal(lower(k = 5, n = 59, alpha = .05, dist = "binom"),
               lowerBinom(59, 5, .05))
})

test_that("U.B. on the number of deviations poisson match", {
  expect_equal(upper(k = 5, n = 59, alpha = .05, dist = "pois"),
               upperPois(59, 5, .05))
})

test_that("U.B. on the number of deviations binomial match", {
  expect_equal(upper(k = 5, n = 59, alpha = .05, dist = "binom"),
               upperBinom(59, 5, .05))
})
