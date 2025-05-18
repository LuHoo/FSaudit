library(testthat)
library(FSaudit)

context("MUS Evaluation")

test_that("MUS cell evaluation hypergeometric match", {
  testResults <-
    mus_eval(cl = 0.95, errorPatternsMUS$BV, errorPatternsMUS$AV4, 1000000,
             dist = "hyper",
             evalMeth = "cell")
  expect_equal(testResults$Over$`Basic precision`, 92112)
  expect_equal(testResults$Over$`Upper confidence bound`, 144863.43)
  expect_equal(testResults$Under$`Upper confidence bound`, 92112)
})

test_that("MUS cell evaluation binomial match", {
  testResults <-
    mus_eval(cl = 0.95, errorPatternsMUS$BV, errorPatternsMUS$AV4, 1000000,
           dist = "binom",
           evalMeth = "cell")
  expect_equal(testResults$Over$`Basic precision`, 92115)
  expect_equal(testResults$Over$`Upper confidence bound`, 144866.43)
  expect_equal(testResults$Under$`Upper confidence bound`, 92115)
})

test_that("MUS cell evaluation poisson match", {
  testResults <-
    mus_eval(cl = 0.95, errorPatternsMUS$BV, errorPatternsMUS$AV4, 1000000,
           dist = "pois",
           evalMeth = "cell")
  expect_equal(testResults$Over$`Basic precision`, 96637)
  expect_equal(testResults$Over$`Upper confidence bound`, 153803.43)
  expect_equal(testResults$Under$`Upper confidence bound`, 96637)
})

bv <- c(accounts_receivable$amount[1:142], 100, 250, 400)
av <- c(accounts_receivable$amount[1:142], 0, 200, 384)

test_that("MUS stringer evaluation hypergeometric match", {
  testResults <- mus_eval(cl = 0.95, bv, av, 13500000,
                          dist = "hyper",
                          evalMeth = "stringer")
  expect_equal(testResults$Over$`Basic precision`, 276050)
  expect_equal(testResults$Over$`Upper confidence bound`, 469616.48)
  expect_equal(testResults$Over$`Precision calculation`$precision,
               c(276050, 66853.55, 9684.91, 1579.74),
               tolerance = 1e-06)
})

test_that("MUS stringer evaluation binomial match", {
  testResults <- mus_eval(cl = 0.95, bv, av, 13500000, dist = "binom",
           evalMeth = "stringer")
  expect_equal(testResults$Over$`Basic precision`, 276052)
  expect_equal(testResults$Over$`Upper confidence bound`, 469619.52)
  expect_equal(testResults$Over$`Precision calculation`$precision,
               c(276052, 66854.55, 9684.91, 1579.78),
               tolerance = 1e-06)
})

test_that("MUS stringer evaluation poisson match", {
  testResults <- mus_eval(cl = 0.95, bv, av, 13500000, dist = "pois",
           evalMeth = "stringer")
  expect_equal(testResults$Over$`Basic precision`, 278914)
  expect_equal(testResults$Over$`Upper confidence bound`, 475998.28)
  expect_equal(testResults$Over$`Precision calculation`$precision,
               c(278914, 69653.55, 10277.31, 1705.14),
               tolerance = 1e-06)
})

test_that("MUS pps estimate", {
  testResults <- mus_eval(0.90, PPS_Example$BV, PPS_Example$AUDIT_AMT,
                          popBv = 7360816, evalMeth = "pps")
  expect_equal(testResults$`pps estimate`, 6402143,
               tolerance = 1e-06)
  expect_equal(testResults$`Lower bound`, 5896179,
               tolerance = 1e-06)
  expect_equal(testResults$`Upper bound`, 6908107,
               tolerance = 1e-06)
})

test_that("Error message when parameters are not numeric", {
  expect_error(mus_eval("253kj", PPS_Example$BV, PPS_Example$AUDIT_AMT, 1000000,
         "abv"))
  expect_error(mus_eval(cl = 0.95, PPS_Example, "ajgh", 0))
  finding <- data.frame(bv = c(rep(5023, 10), "dfg", "poe"),
         av = c(rep("abc", 10), 1236, 5123))
  expect_error(mus_eval(cl = 0.95, finding, 1000000))
})

test_that("Error message when book and audit values are not of equal
          length", {
  a <- c(1, 4, 5, 7, 6, 6, 3)
  b <- c(33, 2)
  expect_error(mus_eval(cl = 0.95, a, b, 1000000))
  expect_error(mus_eval(cl = 0.95, "a", 2, 100000, 0))
})

test_that("Error message when parameter evalMeth is not cell or stringer or
          pps", {
  expect_error(mus_eval(cl = 0.95, PPS_Example$BV, PPS_Example$AUDIT_AMT,
                        1000000, evalMeth = "sting"))
})

test_that("Error message when book value contains both positive and negative
          values", {
  bv <- c(77, 42.56, 69.45, 50, -19.7, 87.4, 1, -11.9, 0)
  av <- rep(55, 9)
  expect_error(mus_eval(cl = 0.95, bv, av, 1000000))
})

test_that("Evaluation ignores book value of significant items", {
  ar <- mus_obj(bv = accounts_receivable$amount,
                id = accounts_receivable$invoice,
                cl = 0.95,
                pm = 45000,
                ee = 10000)
  ar <- size(ar, evalMeth = "stringer")
  original_n <- ar$n
  ar <- select(ar,
               selMeth = "randomized.fixed",
               aggregates = FALSE,
               seed = 1)
  audit_values <- accounts_receivable[match(ar$sample$item,
              accounts_receivable$invoice),
        c("invoice", "av1")]
  ar <- evaluate(ar,
                 av = audit_values$av1,
                 dist = "hyper",
                 evalMeth = "cell")

  bp_from_object <- ar$evalResults$Over$`Basic precision`
  bookValSign <- sum(ar$popSign)

  popBv <- sum(accounts_receivable$amount)
  popBvNonsignif <- round(popBv - bookValSign, 0)

  n_new <- mus_size(.95, popBvNonsignif, 45000, ee = 10000,
                    evalMeth = "stringer")

  # Evaluation on non-significant items only (reduced B and n)
  bp_calculated <- upperHyper(popBvNonsignif, n_new, 0, .05)
  expect_equal(bp_from_object, bp_calculated)
  expect_equal(ar$n, n_new)

})

test_that("MUS cell evaluation AV1", {
  # One 100% overstatement
  testResults <- mus_eval(cl = 0.95, errorPatternsMUS$BV, errorPatternsMUS$AV1,
           popBv = 7360816)
  # Results from IDEA AV1 Cell
  expect_equal(testResults$Over$`Basic precision`, 678026.13,
               tolerance = 1e-04)
  expect_equal(testResults$Over$`Upper confidence bound`, 1060622.35,
               tolerance = 1e-04)
  expect_equal(testResults$Under$`Upper confidence bound`, 678026.13,
               tolerance = 1e-04)
})

test_that("MUS cell evaluation AV2", {
  # One large understatement
  testResults <- mus_eval(cl = 0.95, errorPatternsMUS$BV, errorPatternsMUS$AV2,
           popBv = 7360816)
  # Results from IDEA AV2 Cell
  expect_equal(testResults$Over$`Basic precision`, 678026.13,
               tolerance = 1e-04)
  expect_equal(testResults$Over$`Upper confidence bound`, 678026.13,
               tolerance = 1e-04)
  expect_equal(testResults$Under$`Upper confidence bound`, 916207.89,
               tolerance = 1e-04)
})

test_that("MUS cell evaluation AV3", {
  # One 100% overstatement and one large understatement
  testResults <- mus_eval(cl = 0.95, errorPatternsMUS$BV, errorPatternsMUS$AV3,
           popBv = 7360816)
  # Results from IDEA AV3 Cell
  expect_equal(testResults$Over$`Basic precision`, 678026.13,
               tolerance = 1e-04)
  expect_equal(testResults$Over$`Upper confidence bound`, 1060622.35,
               tolerance = 1e-04)
  expect_equal(testResults$Under$`Upper confidence bound`, 916207.89,
               tolerance = 1e-04)
})

test_that("MUS cell evaluation AV4", {
  # Four overstatements, one 100% and three smaller ones that don't affect PGW
  testResults <- mus_eval(cl = 0.95, errorPatternsMUS$BV, errorPatternsMUS$AV4,
           popBv = 7360816)
  # Results from IDEA AV4 Cell
  expect_equal(testResults$Over$`Basic precision`, 678026.13,
               tolerance = 1e-04)
  expect_equal(testResults$Over$`Upper confidence bound`, 1066321.05,
               tolerance = 1e-04)
  expect_equal(testResults$Under$`Upper confidence bound`, 678026.13,
               tolerance = 1e-04)
})

test_that("MUS cell evaluation AV5", {
  # Three small understatements that don't affect PGW
  testResults <- mus_eval(cl = 0.95, errorPatternsMUS$BV, errorPatternsMUS$AV5,
           popBv = 7360816)
  # Results from IDEA AV5 Cell
  expect_equal(testResults$Over$`Basic precision`, 678026.13,
               tolerance = 1e-04)
  expect_equal(testResults$Over$`Upper confidence bound`, 678026.13,
               tolerance = 1e-04)
  expect_equal(testResults$Under$`Upper confidence bound`, 684009.76,
               tolerance = 1e-04)
})

test_that("MUS cell evaluation AV6", {
  # Five 100% overstatement errors
  testResults <- mus_eval(cl = 0.95, errorPatternsMUS$BV, errorPatternsMUS$AV6,
           popBv = 7360816)
  # Results from IDEA AV6 Cell
  expect_equal(testResults$Over$`Basic precision`, 678026.13,
               tolerance = 1e-04)
  expect_equal(testResults$Over$`Upper confidence bound`, 2279217.31,
               tolerance = 1e-04)
  expect_equal(testResults$Under$`Upper confidence bound`, 678026.13,
               tolerance = 1e-04)
})

test_that("MUS cell evaluation AV7", {
  # All sampling units are 10% overstated
  testResults <- mus_eval(cl = 0.95, errorPatternsMUS$BV, errorPatternsMUS$AV7,
           popBv = 7360816)
  # Results from IDEA AV7 Cell
  expect_equal(testResults$Over$`Basic precision`, 678026.13,
               tolerance = 1e-04)
  expect_equal(testResults$Over$`Upper confidence bound`, 1414107.73,
               tolerance = 1e-04)
  expect_equal(testResults$Under$`Upper confidence bound`, 678026.13,
               tolerance = 1e-04)
})

test_that("MUS cell evaluation AV8", {
  # All sampling units are 10% understated
  testResults <- mus_eval(cl = 0.95, errorPatternsMUS$BV, errorPatternsMUS$AV8,
           popBv = 7360816)
  # Results from IDEA AV8 Cell
  expect_equal(testResults$Over$`Basic precision`, 678026.13,
               tolerance = 1e-04)
  expect_equal(testResults$Over$`Upper confidence bound`, 678026.13,
               tolerance = 1e-04)
  expect_equal(testResults$Under$`Upper confidence bound`, 1414107.73,
               tolerance = 1e-04)
})

test_that("MUS cell evaluation AV9", {
  # Four overstatements, four understatements with one tainting > 100%
  testResults <- mus_eval(cl = 0.95, errorPatternsMUS$BV, errorPatternsMUS$AV9,
           popBv = 7360816)
  # Results from IDEA AV9 Cell
  expect_equal(testResults$Over$`Projected misstatement`, 433700.05,
               tolerance = 1e-04)
  expect_equal(testResults$Under$`Projected misstatement`, 650505.64,
               tolerance = 1e-04)
  expect_equal(testResults$Under$`Upper confidence bound`, 1718940.49,
               tolerance = 1e-04)
})

test_that("MUS cell evaluation AV10", {
  # Nineteen overstatements, twelve understatements with tainting 10%
  testResults <- mus_eval(cl = 0.95, errorPatternsMUS$BV, errorPatternsMUS$AV10,
           popBv = 7360816)
  # Results from IDEA AV10 Cell
  expect_equal(testResults$Over$`Projected misstatement`, 451145.94,
               tolerance = 1e-06)
  expect_equal(testResults$Under$`Projected misstatement`, 284939.82,
               tolerance = 1e-06)
  expect_equal(testResults$Over$`Basic precision`, 678026.13,
               tolerance = 1e-04)
  expect_equal(testResults$Over$`Upper confidence bound`, 1129172.92,
               tolerance = 1e-04)
  expect_equal(testResults$Under$`Upper confidence bound`, 962984.69,
               tolerance = 1e-04)
})

test_that("MUS Stringer evaluation AV1", {
  # One 100% overstatement
  testResults <- mus_eval(cl = 0.95, errorPatternsMUS$BV, errorPatternsMUS$AV1,
           popBv = 7360816,
           evalMeth = "stringer")
  # Results from IDEA AV1 Stringer
  expect_equal(testResults$Over$`Projected misstatement`, 237445.68)
  expect_equal(testResults$Over$`Upper confidence bound`, 1060621)
  expect_equal(testResults$Under$`Upper confidence bound`, 678033)
})

test_that("MUS Stringer evaluation AV2", {
  # One large understatement
  testResults <- mus_eval(cl = 0.95, errorPatternsMUS$BV, errorPatternsMUS$AV2,
                          popBv = 7360816,
                          evalMeth = "stringer")
  # Results from IDEA AV2 Stringer
  expect_equal(testResults$Over$`Projected misstatement`, 0)
  expect_equal(testResults$Over$`Upper confidence bound`, 678033)
})

test_that("MUS Stringer evaluation AV3", {
  # One 100% overstatement and one large understatement
  testResults <- mus_eval(cl = 0.95, errorPatternsMUS$BV, errorPatternsMUS$AV3,
           popBv = 7360816,
           evalMeth = "stringer")
  # Results from IDEA AV3 Stringer
  expect_equal(testResults$Over$`Projected misstatement`, 237445.68)
  expect_equal(testResults$Over$`Upper confidence bound`, 1060621)
})

test_that("MUS Stringer evaluation AV4", {
  # Four overstatements, one 100% and three smaller ones that don't affect PGW
  testResults <- mus_eval(cl = 0.95, errorPatternsMUS$BV, errorPatternsMUS$AV4,
           popBv = 7360816,
           evalMeth = "stringer")
  # Results from IDEA AV4 Stringer
  expect_equal(testResults$Over$`Projected misstatement`, 243153.47)
  expect_equal(testResults$Over$`Upper confidence bound`, 1068230.09)
  expect_equal(testResults$Under$`Upper confidence bound`, 678033)
})

test_that("MUS Stringer evaluation AV5", {
  # Three small understatements that don't affect PGW
  testResults <- mus_eval(cl = 0.95, errorPatternsMUS$BV, errorPatternsMUS$AV5,
           popBv = 7360816,
           evalMeth = "stringer")
  # Results from IDEA AV5 Stringer
  expect_equal(testResults$Over$`Projected misstatement`, 0)
  expect_equal(testResults$Over$`Upper confidence bound`, 678033)
})

test_that("MUS Stringer evaluation AV6", {
  # Five 100% overstatement errors
  testResults <- mus_eval(cl = 0.95, errorPatternsMUS$BV, errorPatternsMUS$AV6,
           popBv = 7360816,
           evalMeth = "stringer")
  # Results from IDEA AV6 Stringer
  expect_equal(testResults$Over$`Projected misstatement`, 1187228.39)
  expect_equal(testResults$Over$`Upper confidence bound`, 2279226)
  expect_equal(testResults$Under$`Upper confidence bound`, 678033)
})

test_that("MUS Stringer evaluation AV7", {
  # All sampling units are 10% overstated
  testResults <- mus_eval(cl = 0.95, errorPatternsMUS$BV, errorPatternsMUS$AV7,
           popBv = 7360816,
           evalMeth = "stringer")
  # Results from IDEA AV7 Stringer
  expect_equal(testResults$Over$`Projected misstatement`, 736085.37)
  expect_equal(testResults$Over$`Upper confidence bound`, 1346319.33)
  expect_equal(testResults$Under$`Upper confidence bound`, 678033)
})

test_that("MUS Stringer evaluation AV8", {
  # All sampling units are 10% understated
  testResults <- mus_eval(cl = 0.95, errorPatternsMUS$BV, errorPatternsMUS$AV8,
           popBv = 7360816,
           evalMeth = "stringer")
  # Results from IDEA AV8 Stringer
  expect_equal(testResults$Over$`Projected misstatement`, 0)
  expect_equal(testResults$Over$`Upper confidence bound`, 678033)
})

test_that("MUS Stringer evaluation AV9", {
  # Four overstatements, four understatements with one tainting > 100%
  testResults <- mus_eval(cl = 0.95, errorPatternsMUS$BV, errorPatternsMUS$AV9,
           popBv = 7360816,
           evalMeth = "stringer")
  # Results from IDEA AV9 Stringer
  expect_equal(testResults$Over$`Projected misstatement`, 433700.05)
  expect_equal(testResults$Under$`Projected misstatement`, 650505.64)
})

test_that("MUS Stringer evaluation AV10", {
  # Nineteen overstatements, twelve understatements with tainting 10%
  testResults <- mus_eval(cl = 0.95, errorPatternsMUS$BV, errorPatternsMUS$AV10,
           popBv = 7360816,
           evalMeth = "stringer")
  # Results from IDEA AV10 Stringer
  expect_equal(testResults$Over$`Projected misstatement`, 451145.94)
  expect_equal(testResults$Under$`Projected misstatement`, 284939.82)
  expect_equal(testResults$Over$`Upper confidence bound`, 1169080.72)
})

test_that("pps evaluation AV9", {
  # Four overstatements, four understatements with one tainting > 100%
  testResults <- mus_eval(cl = 0.95, errorPatternsMUS$BV, errorPatternsMUS$AV9,
                          popBv = 7360816,
                          evalMeth = "pps")
  expect_equal(testResults$`Effective df`, 7)
  bv <- errorPatternsMUS$BV
  av <- errorPatternsMUS$AV9
  sample <- data.frame(bv = bv, av = av)
  sample$error <- sample$bv - sample$av
  sample$taint <- sample$error / sample$bv
  nonzeroes <- sample[sample$error != 0, ]
  m <- length(nonzeroes$bv)
  sd_est <- sd(sample$taint)
  se <- (7360816 * sd_est) / sqrt(31)
  precAch <- se * qt(1 - .05 / 2, m - 1)
  estimate <- 7360816 * (1 / 31) * sum(sample$av / sample$bv)
  expect_equal(testResults$`pps estimate`, estimate)
  expect_equal(testResults$`Upper bound`, estimate + precAch)
})
