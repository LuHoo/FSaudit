library(testthat)
library(FSaudit)

context("Attribute Hypergeometric")

test_that("Attribute sample sizes hypergeometric match", {
  expect_equal(sizeHyper(.1, 5000, 100000, 0), 45)
  expect_equal(sizeHyper(.1, 5000, 100000, 1), 77)
  expect_equal(sizeHyperEe(.1, 5000, 100000, 1000)$n, 77)
})
test_that("Error message when parameters are not numeric", {
  expect_error(sizeHyper("bjkm", "25k", 1000, 0))
  expect_error(sizeHyperEe(.1, TRUE, "jkl", 100)$n)
})
test_that("Error message when parameters alpha <= 0 or >= 1", {
  expect_error(sizeHyper(-2, 10, 1000, 0))
  expect_error(sizeHyper(1, 10, 1000, 0))
  expect_error(sizeHyperEe(-2, 10, 1000, 5)$n)
  expect_error(sizeHyperEe(1, 10, 1000, 5)$n)
})
test_that("Error message when parameters popn is not a positive integer", {
  expect_error(sizeHyper(.05, 10, -1000, 1))
  expect_error(sizeHyper(.1, 500, 6500.25, 1))
  expect_error(sizeHyperEe(.05, 10, -1000, 1)$n)
  expect_error(sizeHyperEe(.1, 500, 6500.25, 100)$n)
})
test_that("Error message when parameter popdev is not a positive integer", {
  expect_error(sizeHyper(.05, -10, 1000, 0))
  expect_error(sizeHyper(.05, 52.5, 2000, 1))
  expect_error(sizeHyperEe(.05, -10, 1000, 1)$n)
  expect_error(sizeHyperEe(.05, 52.5, 2000, 10)$n)
})
test_that("Error message when parameter popdev >= popn ", {
  expect_error(sizeHyper(.1, 5000, 2000, 0))
  expect_error(sizeHyperEe(.1, 5000, 2000, 100))
})
test_that("Error message when parameter c is not a positive integer", {
  expect_error(sizeHyper(.1, 500, 2000, -1))
  expect_error(sizeHyper(.1, 500, 2000, 4.4))
})
test_that("Error message when parameter ee is not a positive integer", {
  expect_error(sizeHyperEe(.1, 500, 2000, -5))
  expect_error(sizeHyperEe(.1, 500, 2000, .04))
})
test_that("Error message when parameter ee >= popdev", {
  expect_error(sizeHyperEe(.1, 500, 2000, 1000)$n)
})

test_that("Idea software and att_size_hypergeometric function result test", {
  testIdea$alpha <- 1 - (testIdea$cl / 100)
  testIdea$popdev <- ceiling((testIdea$tdr / 100) * testIdea$popn)
  testIdea$popn <- as.numeric(testIdea$popn)
  testIdea$new_ee <- ceiling(testIdea$popn * (testIdea$edr / 100))
  nR <- nrow(testIdea)
  for (i in 1 : nR) {
    testIdea$fun_n[i] <- sizeHyperEe(testIdea$alpha[i], testIdea$popdev[i],
                                     testIdea$popn[i], testIdea$new_ee[i])$n
  }
  samSizeCheck <- ifelse(testIdea$n == testIdea$fun_n, TRUE, FALSE)
  expect_true(unique(samSizeCheck) == TRUE)
})

test_that("U.B. on the number of deviations for hypergeometric", {
  expect_equal(upperHyper(500, 59, 0, .05),  23)
  expect_equal(upperHyper(1000, 20, 3, .10), 302)
  expect_equal(upperHyper(50000, 30, 30, .05), 50000)
  # if n = k then resultant is popn
  expect_lte(upperHyper(1000000, 59, 0, .05), 1000000)
  # the resultant is always <= popn
})
test_that("Error message when popn < n", {
  expect_error(upperHyper(100, 120, 15, .10))
})
test_that("Error message when popn is not a positive integer", {
  expect_error(upperHyper(-100, 10, 2, .05))
  expect_error(upperHyper(100.99, 10, 2, .05))
})
test_that("Error message when  parameter n is not a positive integer", {
  expect_error(upperHyper(1000, -12, 1, .05))
  expect_error(upperHyper(1000, 0.99, 0, .05))
})
test_that("Error message when n < k", {
  expect_error(upperHyper(1000, 10, 15, .05))
})
test_that("Error message when k is not a positive integer", {
  expect_error(upperHyper(1000, 10, -1, .05))
  expect_error(upperHyper(1000, 10, -2.65, .05))
})
test_that("Error message when parameter alpha is <= 0 or >= 1", {
  expect_error(upperHyper(1000, 56, 1, -3.5))
  expect_error(upperHyper(1000, 56, 1, 1))
})


test_that("L.B. on fraction of deviations for sample without replacement", {
  expect_equal(lowerHyper(1000000, 59, 6, .05), 45224)
  expect_equal(lowerHyper(201, 101, 48, .75), 100)
  expect_lte(lowerHyper(1000000, 59, 6, .05), 1000000)
  # the resultant is always <= popn
  expect_equal(lowerHyper(1000000, 59, 0, .05), 0)
  # the resultant is always zero when k = 0
})
test_that("Error message when popn < n", {
  expect_error(lowerHyper(100, 120, 15, .10))
})
test_that("Error message when popn is not a positive integer", {
  expect_error(lowerHyper(-100, 10, 2, .05))
  expect_error(lowerHyper(100.99, 10, 2, .05))
})
test_that("Error message when  parameter n is not a positive integer", {
  expect_error(lowerHyper(1000, -12, 1, .05))
  expect_error(lowerHyper(1000, 0.99, 0, .05))
})
test_that("Error message when n < k", {
  expect_error(lowerHyper(1000, 10, 15, .05))
})
test_that("Error message when k is not a positive integer", {
  expect_error(lowerHyper(1000, 10, -1, .05))
  expect_error(lowerHyper(1000, 10, -2.65, .05))
})
test_that("Error message when parameter alpha is <= 0 or >= 1", {
  expect_error(lowerHyper(1000, 56, 1, -3.5))
  expect_error(lowerHyper(1000, 56, 1, 1))

})

test_that("Lower hypergeometric bound greater than Poisson bound", {
  expect_gt(lowerHyper(1000, 100, 1, .05) / 1000, lowerPois(100, 1, .05))
  expect_gt(lowerHyper(10000, 100, 1, .05) / 10000, lowerPois(100, 1, .05))
  expect_gt(lowerHyper(100000, 100, 5, .05) / 100000, lowerPois(100, 5, .05))
})
