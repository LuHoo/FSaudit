library(testthat)
library(FSaudit)

context("MUS Poisson")

sam_bv <- c(19999, 20000, 20002, 20000, 20000, 20002, 20000, 19999, 19999,
            20000, 20001, 20000, 20000, 20000, 19999, 20002, 20000, 19998,
            20001, 19999, 19999, 20000, 19999, 19999, 19999, 19998, 20001,
            20000, 19999, 20000, 20000, 20000, 20001, 20001, 20001, 20001,
            20001, 20000, 20000, 20000, 19999, 20000, 19999, 20002, 20001,
            19999, 20000, 19999, 20001, 20000)

test_that("mus sample sizes poisson match", {
  expect_equal(musSizePois(.95, sam_bv, 100000, 20000,
                           evalMeth = "stringer"), 47) # when ee > 0
  expect_equal(musSizePois(.95, sam_bv, 100000, 0,
                           evalMeth = "stringer"), 30) # when ee = 0
  expect_equal(musSizePois(.95, sam_bv, 100000, 30000,
                           evalMeth = "stringer"), 61)
})
test_that("Error message when parameters are not numeric", {
  expect_error(musSizePois("bjkm", "25k", 1000, 0))
  expect_error(musSizePois(.90, sam_bv, TRUE, 0))
})
test_that("Error message when parameter cl <= 0 or  >= 1", {
  expect_error(musSizePois(-1, sam_bv, 100000, 0))
  expect_error(musSizePois(2, sam_bv, 100000, 0))
})
test_that("Error message when popolation book values are not numeric", {
  testData <- data.frame(sampling_unit = c("A", "B", "C", "D", "E", "F", "G",
                                           "H", "I"),
                         book_value = c("abc", "bgd", rep(213, 7)))
  expect_error(musSizePois(.95, testData$book_value, 100000, 200))
})
test_that("Error message when book value contains both positive and negative
          values", {
  bv <- c(77, 42.56, 69.45, 50, -19.7, 87.4, 1, -11.9, 0)
  expect_error(musSizePois(.95, bv, 100000, 200))
})
test_that("Error message when  parameter ee < 0 or >= pm", {
  expect_error(musSizePois(.95, sam_bv, 100000, -1))
  expect_error(musSizePois(.95, sam_bv, 100001, 200000))
})
test_that("Error message when  parameter pm < 0 or >= pop_bv", {
  expect_error(musSizePois(.95, sam_bv, -100000, 0))
  expect_error(musSizePois(.95, sam_bv, 1000001, 0))
})
