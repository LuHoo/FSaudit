library(testthat)
library(FSaudit)

context("Grammar")

test_that("Check mus size grammar", {
  mus_state <- mus_obj(cl = 0.90, popBv = 1000000, pm = 100000, dist = "pois",
                       evalMeth = "stringer")
  testResults <- mus_state %>% size()
  expect_true(!is.null(testResults$n))
})

test_that("Check cvs size grammar", {
  cvs_state <- cvs_obj(desPrec = 300000, cl = 0.95, sdAv = sd(inventoryData$av),
                       bv = inventoryData$bv, strata = 1,
                       id = inventoryData$item)
  testResults <- cvs_state %>% size()
  expect_true(!is.null(testResults$n))


  cvs_state <- cvs_obj(desPrec = 6188, cl = 0.95, bv = stratData$BV, strata = 4,
                       id = stratData$ITEM)
  testResults <- cvs_state %>% stratify()  %>% size()
  expect_true(!is.null(testResults$n))
})

test_that("Check attribute size grammar", {
  att_state <- att_obj(alpha = .1, popdev = 500,
                       popn = 10000,
                       ee = 100)
  testResults <- att_state %>% size()
  expect_true(!is.null(testResults$n))

  att_state <- att_obj(alpha = .05, tdr = .01,
                       c = 1,
                       dist = "pois")
  testResults <- att_state %>% size()
  expect_true(!is.null(testResults$n))

  att_state <- att_obj(alpha = .05, tdr = .01,
                       eer = 0.001,
                       dist = "binom")
  testResults <- att_state %>% size()
  expect_true(!is.null(testResults$n))
})


test_that("Check mus selection grammar", {
  su <- smallPop$sampling_unit
  bv <- smallPop$book_value
  mus_state <- mus_obj(pm = 25, id = su,
                       bv = bv, dist = "hyper",
                       evalMeth = "stringer")
  testResults <- mus_state %>% size() %>% select()
  expect_true(!is.null(testResults$sample))
})

test_that("Check cvs selection grammar", {
  cvs_state <- cvs_obj(desPrec = 6188, cl = 0.95,
               bv = stratData$BV, strata = 4,
               id = stratData$ITEM)
  testResults <- cvs_state %>% stratify() %>% size() %>% select()
  expect_true(!is.null(testResults$sample))
})


test_that("Check stratify grammar for equal method", {
  cvs_state <- cvs_obj(cl = .90, bv = errorPatternsMUS$BV,
                       strata = 3, id = errorPatternsMUS$ITEM)
  testResults <- cvs_state %>% stratify()
  expect_true(!is.null(testResults$strata))
})

test_that("Check stratify grammar for cumulative method", {
  cvs_state <- cvs_obj(cl = .90, bv = errorPatternsMUS$BV,
                       strata = 3, stratMeth = "cumulative",
                       classes = 5, id = stratData$ITEM)
  testResults <- cvs_state %>% stratify()
  expect_true(!is.null(testResults$strata))
})


test_that("Check significant grammar", {
  su <- inventoryData$item
  bv <- inventoryData$bv
  mus_state <- mus_obj(pm = 10000, id = su,
                       bv = bv)
  testResults <- mus_state %>% significant()
  expect_true(!is.null(testResults$signItems))
})


test_that("Check mus eval grammar", {
  su <- errorPatternsMUS$ITEM
  bv <- errorPatternsMUS$BV
  mus_state <- mus_obj(pm = 25000, id = su,
                       bv = bv, evalMeth = "stringer")

  testResults1 <- mus_state %>% size() %>% select()
  av <- errorPatternsMUS[match(testResults1$sample$item, errorPatternsMUS$ITEM),
                         "AV1"]

  testResults <- testResults1 %>% evaluate(av = av)
  expect_true(!is.null(testResults$evalResults))
})

test_that("Check cvs eval grammar", {
  cvs_state <- cvs_obj(desPrec = 6188, cl = 0.95,
            bv = errorPatternsMUS$BV, strata = 4,
            id = errorPatternsMUS$ITEM)
  testResults1 <- cvs_state %>% stratify() %>% size() %>% select()
  av <- errorPatternsMUS[match(testResults1$sample$item, errorPatternsMUS$ITEM),
                         "AV8"]
  testResults <- testResults1 %>% evaluate(av = av)
  expect_true(!is.null(testResults$evalResults))
})

test_that("Check attribute size grammar sample size", {
  att_state <- att_obj(alpha = .1, popdev = 500,
                       popn = 10000, ee = 100)
  testResults <- att_state %>% size()
  funResult <- testResults$n
  output <- sizeHyperEe(alpha = .1, popdev = 500, popn = 10000, ee = 100)[[1]]
  expect_true(all.equal(funResult, output))

  att_state <- att_obj(alpha = .05, popdev = 500,
                       popn = 10000, c = 1)
  testResults <- att_state %>% size()
  funResult <- testResults$n
  output <- sizeHyper(alpha = .05, popdev = 500, popn = 10000, c = 1)
  expect_true(all.equal(funResult, output))

  att_state <- att_obj(alpha = .1, tdr = .05,
                       c = 2, dist = "binom")
  testResults <- att_state %>% size()
  funResult <- testResults$n
  output <- sizeBinom(alpha = .1, tdr = .05, c = 2)
  expect_true(all.equal(funResult, output))

  att_state <- att_obj(alpha = .1, tdr = .05,
                       eer = .005, dist = "binom")
  testResults <- att_state %>% size()
  funResult <- testResults$n
  output <- sizeBinomEer(alpha = .1, tdr = .05, eer = .005)[[1]]
  expect_true(all.equal(funResult, output))

  att_state <- att_obj(alpha = 0.10, tdr = 0.05,
                       c = 2, dist = "pois")
  testResults <- att_state %>% size()
  funResult <- testResults$n
  output <- sizePois(alpha = .1, tdr = .05, c = 2)
  expect_true(all.equal(funResult, output))

  att_state <- att_obj(alpha = 0.10, tdr = 0.05,
                       eer = .005, dist = "pois")
  testResults <- att_state %>% size()
  funResult <- testResults$n
  output <- sizePoisEer(alpha = .1, tdr = .05, eer = 0.005)[[1]]
  expect_true(all.equal(funResult, output))
  })


test_that("Test that sample size does not exceed population size", {
  att_state <- att_obj(alpha = .1, tdr = 0.05,
                       popn = 30, c = 0)
  att_sample <- size(att_state, dist = "binom")
  expect_equal(att_sample$n, 30)


  pension <- att_obj(alpha = .1,
                     c = 0)
  pension <- size(pension, popdev = 1, popn = 40,
                  dist = "hyper")
  expect_equal(pension$n, 37)
})
