library(testthat)
library(FSaudit)
library(tibble)

context("MUS Sampling")

test_that("samples units based on random method", {

  selection <-  mus_select(id = smallPop$sampling_unit,
                           bv = smallPop$book_value,
                           n = 5,
                           aggregates = TRUE,
                           selMeth = "random",
                           seed = 123)

  selectSamples <- as.factor(c("C", "H", "D", "H", "I"))
  monetary <- c(4, 6, 9, 6, 5)
  checkSample <- c()
  checkMonetary <- c()
  for (i in seq_len(length(selection$sample$bv))) {
    checkSample[i] <-
      ifelse(selection$sample$item[i] == selectSamples[i], TRUE, FALSE)
    checkMonetary[i] <-
      ifelse(selection$sample$bv[i] == monetary[i], TRUE, FALSE)
  }
  expect_true(unique(checkSample) == TRUE)
  expect_true(unique(checkMonetary) == TRUE)
})

test_that("samples units based on fixed method", {


  selection <-  mus_select(id = smallPop$sampling_unit,
                           bv = smallPop$book_value,
                           n = 5,
                           aggregates = TRUE,
                           selMeth = "fixed",
                           seed = 123)
  selectFun <- selection$sample$item
  selectMonetary <- selection$sample$bv

  selectSamples <- c("A", "C", "D", "F", "H")
  monetary <- c(7, 4, 9, 8, 6)
  checkSample <- c()
  checkMonetary <- c()
  for (i in seq_len(length(selectFun))) {

    checkSample[i] <- ifelse(selectFun[i] == selectSamples[i], TRUE, FALSE)
    checkMonetary[i] <- ifelse(selectMonetary[i] == monetary[i], TRUE, FALSE)
  }
  expect_true(unique(checkSample) == TRUE)
  expect_true(unique(checkMonetary) == TRUE)
})

test_that("samples units based on randomized.fixed method", {

  selection <-  mus_select(smallPop$sampling_unit,
                           smallPop$book_value, 5,
                           aggregates = TRUE,
                           selMeth = "randomized.fixed", 123)
  selectFun <- selection$sample$item
  selectMonetary <- selection$sample$bv

  selectSamples <- c("F", "A", "G", "D", "H")
  monetary <- c(8, 7, 2, 9, 6)
  checkSample <- c()
  checkMonetary <- c()
  for (i in seq_len(length(selectFun))) {
    checkSample[i] <- ifelse(selectFun[i] == selectSamples[i], TRUE, FALSE)
    checkMonetary[i] <- ifelse(selectMonetary[i] == monetary[i], TRUE, FALSE)
  }
  expect_true(unique(checkSample) == TRUE)
  expect_true(unique(checkMonetary) == TRUE)
})

test_that("samples units based on cell method", {


  selection <-  mus_select(smallPop$sampling_unit,
                           smallPop$book_value, 5,
                           aggregates = TRUE,
                           selMeth = "cell", 123)
  selectFun <- selection$sample$item
  selectMonetary <- selection$sample$bv

  selectSamples <- c("A", "D", "D", "H", "I")
  monetary <- c(7, 9, 9, 6, 5)
  checkSample <- c()
  checkMonetary <- c()
  for (i in seq_len(length(selectFun))) {
    checkSample[i] <- ifelse(selectFun[i] == selectSamples[i], TRUE, FALSE)
    checkMonetary[i] <- ifelse(selectMonetary[i] == monetary[i], TRUE, FALSE)
  }
  expect_true(unique(checkSample) == TRUE)
  expect_true(unique(checkMonetary) == TRUE)
})

test_that("samples units based on sieve method", {

  selection <- mus_select(smallPop$sampling_unit,
                          smallPop$book_value, 5,
                          aggregates = TRUE,
                          selMeth = "sieve", 123)
  selectFun <- selection$sample$item
  selectMonetary <- selection$sample$bv

  selectSamples <- c("A", "C", "D", "F", "I")
  monetary <- c(7, 4, 9, 8, 5)
  checkSample <- c()
  checkMonetary <- c()
  for (i in seq_len(length(selectFun))) {
    checkSample[i] <- ifelse(selectFun[i] == selectSamples[i], TRUE, FALSE)
    checkMonetary[i] <- ifelse(selectMonetary[i] == monetary[i], TRUE, FALSE)
  }
  expect_true(unique(checkSample) == TRUE)
  expect_true(unique(checkMonetary) == TRUE)
})

test_that("samples units based on modified.sieve method", {

  selection <-  mus_select(smallPop$sampling_unit,
                           smallPop$book_value, 5,
                           aggregates = TRUE,
                              selMeth = "modified.sieve", 123)
  selectFun <- selection$sample$item
  selectMonetary <- selection$sample$bv

  selectSamples <- c("F", "A", "D", "C", "I")
  monetary <- c(8, 7, 9, 4, 5)
  checkSample <- c()
  checkMonetary <- c()
  for (i in seq_len(length(selectFun))) {
    checkSample[i] <- ifelse(selectFun[i] == selectSamples[i], TRUE, FALSE)
    checkMonetary[i] <- ifelse(selectMonetary[i] == monetary[i], TRUE, FALSE)
  }
  expect_true(unique(checkSample) == TRUE)
  expect_true(unique(checkMonetary) == TRUE)
})

test_that("samples selection for negative book values", {

  id <- testbv$item
  bv <- testbv$bv

  sp_fun <- split_population(id, bv)

  selection <- mus_select(sp_fun$neg_bv$item, sp_fun$neg_bv$bv, 1,
                          aggregates = TRUE,
                          selMeth = "fixed", 1234)
  expect_equal(selection$sample$item, 103)
  expect_equal(selection$sample$bv, 1499.06)
})

test_that("Error message when population book values are not numeric", {
  corruptFile <-
    data.frame(sampling_unit = c("A", "B", "C", "D", "E", "F", "G", "H", "I"),
               book_value = c("abc", "bgd", rep(213, 7)))
  expect_error(mus_select(corruptFile$sampling_unit,
                          corruptFile$book_value, 5))
})
test_that("Error message when length of book value and sample units differ", {
  su <- c("A", "B", "C", "D", "E", "F", "G", "H", "I")
  bv <- c(7, 2, 6, 5, 9, 8, 1)
  expect_error(mus_select(su, bv, 5))
})
test_that("Error message when  parameter n is not a positive integer", {
  expect_error(mus_select(smallPop$sampling_unit,
                          smallPop$book_value, 5.6))
  expect_error(mus_select(smallPop$sampling_unit,
                          smallPop$book_value, -4))
})
test_that("Error message when  parameter n >= population size", {
  expect_error(mus_select(smallPop$sampling_unit, smallPop$book_value,
                             nrow(smallPop)))
})
test_that("Error message when book value contains both positive and negative
          values", {
  su <- c("A", "B", "C", "D", "E", "F", "G", "H", "I")
  bv <- c(77, 42.56, 69.45, 50, -19.7, 87.4, 1, -11.9, 0)
  expect_error(mus_select(su, bv, 5))
})

sam_bv <- c(19999, 20000, 20002, 20000, 20000, 20002, 20000, 19999, 19999,
            20000, 20001, 20000, 20000, 20000, 19999, 20002, 20000, 19998,
            20001, 19999, 19999, 20000, 19999, 19999, 19999, 19998, 20001,
            20000, 19999, 20000, 20000, 20000, 20001, 20001, 20001, 20001,
            20001, 20000, 20000, 20000, 19999, 20000, 19999, 20002, 20001,
            19999, 20000, 19999, 20001, 20000)


test_that("mus sample sizes match and is always > 0 for binomial", {
  expect_equal(mus_size(.95, sam_bv, 100000, 20000,
                        evalMeth = "stringer", dist = "binom"), 44)
  # when ee > 0
  expect_equal(mus_size(.95, sam_bv, 100000, dist = "binom",
                        evalMeth = "stringer"), 29)
  # when ee = 0

})
test_that("mus sample sizes match and is always > 0 for poisson", {
  expect_equal(mus_size(.95, sam_bv, 100000, 20000, dist = "pois",
                        evalMeth = "stringer"), 47)
  # when ee > 0
  expect_equal(mus_size(.95, sam_bv, 100000, dist = "pois",
                        evalMeth = "stringer"), 30)
  # when ee = 0
})
test_that("mus sample sizes match and is always > 0 for hypergeometric", {
  expect_equal(mus_size(.95, sam_bv, 100000, 20000, dist = "hyper",
                        evalMeth = "stringer"), 44)
  # when ee > 0
  expect_equal(mus_size(.95, sam_bv, 100000, dist = "hyper",
                        evalMeth = "stringer"), 29)
  # when ee = 0

})
test_that("mus sample sizes binomial match", {
  expect_equal(mus_size(.95, sam_bv, 100000, 20000, dist = "binom",
                        evalMeth = "stringer"),
               musSizeBinom(.95, sam_bv, 100000, 20000,
                            evalMeth = "stringer")) # when ee > 0
  expect_equal(mus_size(.95, sam_bv, 100000, dist = "binom",
                        evalMeth = "stringer"),
               musSizeBinom(.95, sam_bv, 100000, 0,
                            evalMeth = "stringer")) # when ee = 0
})
test_that("mus sample sizes poisson match", {
  expect_equal(mus_size(.95, sam_bv, 100000, 20000, dist = "pois",
                        evalMeth = "stringer"),
               musSizePois(.95, sam_bv, 100000, 20000,
                           evalMeth = "stringer")) # when ee > 0
  expect_equal(mus_size(.95, sam_bv, 100000, dist = "pois",
                        evalMeth = "stringer"),
               musSizePois(.95, sam_bv, 100000, 0,
                           evalMeth = "stringer")) # when ee = 0
})
test_that("mus sample sizes hypergeometric match", {
  expect_equal(mus_size(.95, sam_bv, 100000, 20000, dist = "hyper",
                        evalMeth = "stringer"),
               musSizeHyper(.95, sam_bv, 100000, 20000,
                            evalMeth = "stringer")) # when ee > 0
  expect_equal(mus_size(.95, sam_bv, 100000, dist = "hyper",
                        evalMeth = "stringer"),
               musSizeHyper(.95, sam_bv, 100000, 0,
                            evalMeth = "stringer")) # when ee = 0
})


test_that("Individually significant items", {

  set.seed(123)
  selection <- mus_significant(id = smallPop$sampling_unit,
                               bv = smallPop$book_value,
                               pm = 20,
                               dist = "hyper",
                               evalMeth = "stringer")
  indSignItem <- as.character(selection$significant$item)
  indSignBv <- selection$significant$bv
  nonSignItem <- as.character(selection$nonsignificant$item)
  nonSignBv <- selection$nonsignificant$bv

  selectItem <- c("D")
  selectBv <- c(9)
  selectSamples <- c("A", "B", "C", "E", "F", "G", "H", "I")
  monetary <- c(7, 3, 4, 1, 8, 2, 6, 5)

  checkIndSignItem <- c()
  checkIndSignBv <- c()
  for (i in seq_len(length(indSignItem))) {
    checkIndSignItem[i] <- ifelse(indSignItem[i] == selectItem[i], TRUE,
                                  FALSE)
    checkIndSignBv[i] <- ifelse(indSignBv[i] == selectBv[i], TRUE,
                                FALSE)
  }
  expect_true(unique(checkIndSignItem) == TRUE)
  expect_true(unique(checkIndSignBv) == TRUE)

  checkSample <- c()
  checkMonetary <- c()
  for (i in seq_len(length(nonSignItem))) {
    checkSample[i] <- ifelse(nonSignItem[i] == selectSamples[i], TRUE,
                             FALSE)
    checkMonetary[i] <- ifelse(nonSignBv[i] == monetary[i], TRUE,
                               FALSE)
  }
  expect_true(unique(checkSample) == TRUE)
  expect_true(unique(checkMonetary) == TRUE)
})
test_that("Error message when population book values are not numeric", {
  corruptFile <- data.frame(su = smallPop$sampling_unit,
                            bv = c("abc", "bgd", rep(213, 7)))
  expect_error(mus_significant(corruptFile$su,
                               corruptFile$bv, 20))
})
test_that("Error message when length of book value and sample units differ", {
  su <- c("A", "B", "C", "D", "E", "F", "G", "H", "I")
  bv <- c(7, 2, 6, 5, 9, 8, 1)
  expect_error(mus_significant(su, bv, 20))
})
test_that("Error message when book value contains both positive and negative
          values", {
  su <- c("A", "B", "C", "D", "E", "F", "G", "H", "I")
  bv <- c(77, 42.56, 69.45, 50, -19.7, 87.4, 1, -11.9, 0)
  expect_error(mus_significant(su, bv, 20))
})

test_that("Error message when population book values are not numeric", {
  corruptFile <-
    data.frame(sampling_unit = c("A", "B", "C", "D", "E", "F", "G", "H", "I"),
               book_value = c("abc", "bgd", rep(213, 7)))
  expect_error(mus_extend_n(corruptFile$sampling_unit,
                            corruptFile$book_value,
                            sign_items, selection, pm, cl = 0.95,
                            ee = 350515.05, dist = "hyper",
                            selMeth = "fixed"))
})
test_that("Error message when length of book value and sample units differ", {
  su <- c("A", "B", "C", "D", "E", "F", "G", "H", "I")
  bv <- c(7, 2, 6, 5, 9, 8, 1)
  expect_error(mus_extend_n(su, bv, sign_items, selection, pm, cl = 0.95,
                            ee = 350515.05,
                            dist = "hyper", selMeth = "fixed"))
})
test_that("Error message when significant items and non significant items are
          not list of df", {
  sign_items <- data.frame(sampling_unit = c("A", "B", "C", "D", "E"),
                           book_value = c(1168.85, 189.96, 188.88, 715.87,
                                          249.35))
  expect_error(mus_extend_n(su, bv, sign_items, selection, pm, cl = 0.95,
                            ee = 350515.05, dist = "hyper",
                            selMeth = "fixed"))
})

testSample <- inventoryData
pm <- 20000
n <- mus_size(cl = 0.95, testSample$bv, pm, ee = 0, dist = "hyper",
              evalMeth = "stringer")



selection <-  mus_select(testSample$item, testSample$bv, n, aggregates = TRUE,
                         selMeth = "fixed", 123)


test_that("Error message when book value contains both positive and negative
          values", {
            su <- c("A", "B", "C", "D", "E", "F", "G", "H", "I")
            bv <- c(77, 42.56, 69.45, 50, -19.7, 87.4, 1, -11.9, 0)
            expect_error(mus_extend_n(su, bv, sign_items, selection, pm,
                                      cl = 0.95, ee = 350515.05,
                                      dist = "hyper",
                                      selMeth = "fixed"))
})


test_that("Split book values into positive, negative and zero", {

  su <- 1:10
  bv <- c(1000.26, 9999.86, 0, 1236, 7563.6, -123.65, -1493.12, -58.14,
          -5.96, 26.78)
  funResult <- split_population(su, bv)

  row.names(funResult[[1]]) <- seq_len(nrow(funResult[[1]]))

  output1 <- data.frame(no_bv = c(5, 4, 1, 10),
                        sum_bv = c(19826.50, -1680.87, 0.00, 18145.63))
  expect_true(all.equal(funResult[[1]], output1))


  output2 <- tibble(item = as.integer(c(1, 2, 4, 5, 10)),
                    book_value = c(1000.26, 9999.86, 1236, 7563.6, 26.78))
  expect_true(all.equal(funResult[[2]], output2, check.attributes = FALSE))


  output3 <- tibble(item = as.integer(c(6, 7, 8, 9)),
                    book_value = c(-123.65, -1493.12, -58.14, -5.96))
  expect_true(all.equal(funResult[[3]], output3, check.attributes = FALSE))


  output4 <- tibble(item = as.integer(c(3)), book_value = c(0))
  expect_true(all.equal(funResult[[4]], output4, check.attributes = FALSE))
})
test_that("Error message when population book values are not numeric", {
  corruptFile <-
    data.frame(sampling_unit = c("A", "B", "C", "D", "E", "F", "G", "H", "I"),
               book_value = c("abc", "bgd", rep(213, 7)))
  expect_error(split_population(corruptFile$sampling_unit,
                                corruptFile$book_value))
})
test_that("Error message when length of book value and sample units differ", {
  su <- c("A", "B", "C", "D", "E", "F", "G", "H", "I")
  bv <- c(7, 2, 6, 5, 9, 8, 1)
  expect_error(split_population(su, bv))
})
