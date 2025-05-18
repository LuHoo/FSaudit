library(testthat)
library(FSaudit)

context("MUS Binomial")

sam_bv <- c(19999, 20000, 20002, 20000, 20000, 20002, 20000, 19999, 19999,
            20000, 20001, 20000, 20000, 20000, 19999, 20002, 20000, 19998,
            20001, 19999, 19999, 20000, 19999, 19999, 19999, 19998, 20001,
            20000, 19999, 20000, 20000, 20000, 20001, 20001, 20001, 20001,
            20001, 20000, 20000, 20000, 19999, 20000, 19999, 20002, 20001,
            19999, 20000, 19999, 20001, 20000)

test_that("mus sample sizes binomial match", {
  expect_equal(musSizeBinom(.95, sam_bv, 100000, 20000,
                            evalMeth = "stringer"), 44) # when ee > 0
  expect_equal(musSizeBinom(.95, sam_bv, 100000, 0,
                            evalMeth = "stringer"), 29) # when ee = 0
  expect_equal(musSizeBinom(.95, sam_bv, 100000, 40000,
                            evalMeth = "stringer"), 78)
})
test_that("Error message when parameters are not numeric", {
  expect_error(musSizeBinom("bjkm", "25k", 1000, 0,
                            evalMeth = "stringer"))
  expect_error(musSizeBinom(.90, sam_bv, TRUE, 0,
                            evalMeth = "stringer"))
})
test_that("Error message when parameter cl <= 0 or  >= 1", {
  expect_error(musSizeBinom(-1, sam_bv, 100000, 0))
  expect_error(musSizeBinom(2, sam_bv, 100000, 0))
})
test_that("Error message when popolation book values are not numeric", {
  testData <- data.frame(sampling_unit = c("A", "B", "C", "D", "E", "F", "G",
                                           "H", "I"),
                         book_value = c("abc", "bgd", rep(213, 7)))
  expect_error(musSizeBinom(.95, testData$book_value, 100000, 200))
})
test_that("Error message when book value contains both positive and negative
          values", {
  bv <- c(77, 42.56, 69.45, 50, -19.7, 87.4, 1, -11.9, 0)
  expect_error(musSizeBinom(.95, bv, 100000, 200))
})
test_that("Error message when  parameter ee < 0 or >= pm", {
  expect_error(musSizeBinom(.95, sam_bv, 100000, -1))
  expect_error(musSizeBinom(.95, sam_bv, 100000, 200000))
})
test_that("Error message when  parameter pm < 0 or >= pop_bv", {
  expect_error(musSizeBinom(.95, sam_bv, -100000, 0))
  expect_error(musSizeBinom(.95, sam_bv, 1000000, 0))
})
