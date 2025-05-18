library(testthat)
library(FSaudit)

context("DAfA Classical Variables Sampling")

popN <- nrow(inventoryData)
invSample <- cvs_obj(desPrec = 300000,
                     n = 400,
                     sdAv = sd(inventoryData$av),
                     bv = inventoryData$bv,
                     id = inventoryData$item,
                     seed = 12345)

invSample <-  select(invSample)

audit_values <- inventoryData[
  match(invSample$sample$item,
        inventoryData$item), "av"]
invSample <- evaluate(invSample,
                      av = audit_values)

myData <- data.frame(item = invSample$sample$item,
                     bv = invSample$sample$bv,
                     av = audit_values)
myData$bvav <- myData$bv * myData$av
myData$bv2 <- myData$bv * myData$bv
myData$av2 <- myData$av * myData$av
myData$diff <- myData$bv - myData$av
myData$diff2 <- myData$diff * myData$diff

popAvBar <- nrow(inventoryData) * mean(myData$av)
n <- 400
fpc <- sqrt((popN - n) / (popN))
precision <- popN * qt(.975, df = n - 1) * sd(myData$av) / sqrt(n)  * fpc

# Sample results

test_that("Sample results", {

  expect_equal(sum(myData$bvav), 3550437464, tolerance = 1e-06)
  expect_equal(sum(myData$bv2), 3586086982, tolerance = 1e-06)
  expect_equal(sum(myData$av2), 3649977984, tolerance = 1e-06)
  expect_equal(sum(myData$diff2), 135190037, tolerance = 1e-06)

  expect_equal(sum(myData$bv), 863201.37, tolerance = 1e-06)
  expect_equal(sum(myData$av), 864212.48, tolerance = 1e-06)
  expect_equal(sum(myData$diff), -1011.11, tolerance = 1e-06)

})

# Paragraph 4.1

test_that("MPU Estimator", {

  expect_equal(sd(myData$av), 2113.82, tolerance = 1e-05)
  expect_equal(mean(myData$av), 2160.53, tolerance = 1e-06)
  expect_equal(invSample$evalResults$Estimates$mpu[1],
               popAvBar, tolerance = 1e-06)
  expect_equal(invSample$evalResults$Estimates$mpu[1],
               7561859, tolerance = 1e-06)
  expect_equal(precision, 684415, tolerance = 1e-06)
  expect_equal(invSample$evalResults$Estimates$mpu[3],
               precision, tolerance = 1e-06)
  expect_equal(popAvBar - precision, 6877444, tolerance = 1e-06)
  expect_equal(popAvBar + precision, 8246274, tolerance = 1e-06)

})

# Paragraph 4.2

test_that("Regression estimator", {

  slope <- (sum(myData$bvav) - sum(myData$bv) *
              sum(myData$av) / n) / (sum(myData$bv2) -
                                       sum(myData$bv) ^ 2 / n)

  expect_equal(slope, 0.9780, tolerance = 1e-4)

  popAvReg <- popN * mean(myData$av) + slope * (sum(inventoryData$bv) -
                                               popN * mean(myData$bv))

  expect_equal(popAvReg, invSample$evalResults$Estimates$regr[1])
  expect_equal(popAvReg, 7373882, tolerance = 1e-06)

  adj_factor <-
    sqrt(1 - ((sum(myData$bvav) - (sum(myData$bv) * sum(myData$av) / n)) /
                ((n - 1) * sd(myData$bv) * sd(myData$av))) ^ 2)

  expect_equal(adj_factor, 0.2745, tolerance = 1e-4)

  expect_equal(precision * adj_factor * sqrt((n - 1) / (n - 2)),
               invSample$evalResults$Estimates$regr[3])
  expect_equal(precision * adj_factor * sqrt((n - 1) / (n - 2)), 188123,
               tolerance = 1e-05)

})

# Paragraph 4.3

test_that("Difference estimator", {

  diff_est <- sum(inventoryData$bv) - popN * mean(myData$diff)

  expect_equal(diff_est, invSample$evalResults$Estimates$diff[1])
  expect_equal(diff_est, 7369663, tolerance = 1e-06)
  expect_equal(sd(myData$diff), 582.08, tolerance = 1e-05)

  precision_diff <- popN * qt(.975, df = n - 1) * sd(myData$diff) / sqrt(n) *
    fpc
  expect_equal(precision_diff, 188467.44, tolerance = 1e-05)

})

# Paragraph 4.4

test_that("Ratio estimator", {

  ratio <- sum(myData$av) / sum(myData$bv)
  expect_equal(ratio, 1.00117, tolerance = 1e-5)

  ratio_est <- sum(inventoryData$bv) * ratio

  expect_equal(ratio_est, invSample$evalResults$Estimates$ratio[1])
  expect_equal(ratio_est, 7369438, tolerance = 1e-06)

  sd_ratio <- sqrt((sum(myData$av2) - 2 * ratio * sum(myData$bvav) +
                       ratio^2 * sum(myData$bv2)) / (n - 1))
  expect_equal(sd_ratio, 582.27, tolerance = 1e-05)


  precision_ratio <- popN * qt(.975, df = n - 1) * sd_ratio /
    sqrt(n) * fpc
  expect_equal(precision_ratio, invSample$evalResults$Estimates$ratio[3])
  expect_equal(precision_ratio, 188530, tolerance = 1e-06)

})

# Paragraph 4.7

test_that("Regression estimator with 3-9 errors", {
  arSample <- cvs_obj(bv = accounts_receivable$amount,
                      id = accounts_receivable$invoice,
                      n = 100)
  arSample <- select(arSample, seed = 1)
  audit_values <- accounts_receivable[match(arSample$sample$item,
                                            accounts_receivable$invoice), "av2"]
  arSample <- evaluate(arSample, av = audit_values$av2)
  expect_equal(arSample$evalResults$Estimates[3, 4], 532846.0454)
})
