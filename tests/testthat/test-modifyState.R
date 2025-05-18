library(testthat)
library(FSaudit)

context("Modify state")


#' test the "modifyState"

#' Purpose: ========= For a given key and value, modifyState function will
#' re-assign the values to the state. For certain specific values it prepopulate
#' other keys.

#' Testing strategy ================= Expectation: If we supply new values to a
#' parameter, it should re-set the values. Expectation: If we supply a new key
#' which is not part of the sate, the modifyState should return the stae without
#' any change. Also it should provide a gentle warning message without breaking.
#' Expectation: If we supply empty arguments, the modifyState should return the
#' sate as it is.

test_that("Reset the parameters of MUS based on new key value", {
  testResults <- mus_obj(cl = 0.90, popBv = 1000000, pm = 100000, dist = "pois")
  expect_equal(testResults$cl, 0.90)
  expect_equal(testResults$popBv, 1000000)
  expect_equal(testResults$pm, 100000)
  expect_equal(testResults$dist, "pois")
})
test_that("Warning message when a wrong key is supplied to MUS object", {
  expect_warning(mus_obj(cl = 0.90, popBv = 1000000, pm = 100000,
                         dist = "pois", bla = "bla-bla"))
})
test_that("MUS state returns the parameters as it is when empty arguments are
          supplied", {
  testResults <- mus_obj(cl = "", popBv = 1000000, pm = "", dist = "pois")
  expect_equal(testResults$cl, 0.95)
  expect_equal(testResults$popBv, 1000000)
  expect_equal(testResults$pm, NULL)
  expect_equal(testResults$dist, "pois")
})


test_that("Reset the parameters of CVS based on new key value", {
  testResults <- cvs_obj(cl = 0.90, popBv = 1000000,
                         strata = 5, stratMeth = "equal")
  expect_equal(testResults$cl, 0.90)
  expect_equal(testResults$popBv, 1000000)
  expect_equal(testResults$strata, 5)
  expect_equal(testResults$stratMeth, "equal")
})
test_that("Warning message when a wrong key is supplied to CVS object", {
  expect_warning(cvs_obj(cl = 0.90, popBv = 1000000, classes = 4,
                         stratMeth = "cumulative", bla = "bla-bla"))
})
test_that("CVS state returns the parameters as it is when empty arguments are
          supplied", {
  testResults <- cvs_obj(cl = "", popBv = 1000000, strat = "",
                         stratMeth = "cumulative")
  expect_equal(testResults$cl, 0.95)
  expect_equal(testResults$popBv, 1000000)
  expect_equal(testResults$strat, NULL)
  expect_equal(testResults$stratMeth, "cumulative")
})


test_that("Reset the parameters of attribute object based on new key value", {
  testResults <- att_obj(alpha = 0.05, popn = 10000, tdr = 0.01, dist = "pois")
  expect_equal(testResults$alpha, 0.05)
  expect_equal(testResults$popn, 10000)
  expect_equal(testResults$tdr, 0.01)
  expect_equal(testResults$dist, "pois")
})
test_that("Warning message when a wrong key is supplied to attribute object", {
  expect_warning(att_obj(alpha = 0.05, popn = 10000, tdr = 0.01,
                         dist = "pois", bla = "bla-bla"))
})
test_that("attribute state returns the parameters as it is when empty arguments
          are supplied", {
  testResults <- att_obj(alpha = "", popn = 10000, tdr = 0.01, dist = "")
  expect_equal(testResults$alpha, NULL)
  expect_equal(testResults$popn, 10000)
  expect_equal(testResults$tdr, 0.01)
  expect_equal(testResults$dist, "hyper")
})
