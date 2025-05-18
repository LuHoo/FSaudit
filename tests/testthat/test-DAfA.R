library(testthat)
library(FSaudit)

context("DAfA Attribute Sampling")

test_that("Attribute sample sizes hypergeometric match", {
  expect_equal(sizeHyper(.1, 60, 1200, 0), 45)
  expect_equal(sizeHyper(.1, 60, 1200, 1), 75)
  expect_equal(sizeHyper(.1, 60, 1200, 2), 102)
})

test_that("Attribute sample sizes Binomial match", {
  expect_equal(sizeBinom(.1, .05, 2), 105)
})

test_that("Attribute sample sizes poisson match", {
  expect_equal(sizePois(.1, .05, 2), 107)
})

test_that("U.B. on the number of deviations for the sample", {
  expect_equal(upper(0, 1200, 102, .1),  25)
  expect_equal(upper(1, 1200, 102, .1),  43)
  expect_equal(upper(2, 1200, 102, .1),  59)
  expect_equal(upper(3, 1200, 102, .1),  75)
})

# 5.5.2 Monetary Unit Sampling
context("DAfA MUS Sampling")
subsidies_bv <- rep(10000, 1200)
test_that("MUS sample sizes match", {

  expect_equal(mus_size(.95, subsidies_bv, 120000, dist = "hyper",
                        evalMeth = "stringer"), 299)
  # when ee = 0
  expect_equal(sizeHyper(alpha = .05,
                         popn = sum(subsidies_bv),
                         popdev = 120000,
                         c = 1),
               473)
  # when ee > 0
})

# 5.5.3 Accounts receivable confirmation

test_that("MUS sample sizes match", {
  expect_equal(mus_size(.95, accounts_receivable$amount,
                                450000, ee = 100000, dist = "hyper",
                        evalMeth = "stringer"), 145)
})

test_that("samples units based on randomized.fixed method", {
  selection <-  mus_select(accounts_receivable$invoice,
                           accounts_receivable$amount,
                           n = 145,
                           selMeth = "randomized.fixed",
                           seed = 12345,
                           pm = 450000,
                           ee = 100000,
                           dist = "hyper",
                           evalMeth = "stringer")
  sample <- selection[[1]]
  itemNumbers <- sample[, 1]
  bookValues <- sample[, 2]

  expectedItems <-
    c("201711151", "201720628", "201726641", "201727494", "201711802",
      "201703009", "201703086", "201703001", "201701092", "201711888",
      "201738907", "201712571", "201712931", "201712074", "201706699",
      "201718510", "201710278", "201708390", "201724365", "201720682",
      "201728767", "201721136", "201729596", "201735709", "201711844",
      "201733527", "201736681", "201731303", "201712372", "201726719",
      "201709454", "201707637", "201733812", "201736007", "201724407",
      "201736145", "201705280", "201724242", "201720241", "201729587",
      "201713210", "201725173", "201711619", "201716393", "201700023",
      "201729777", "201717742", "201724402", "201727358", "201709243",
      "201732008", "201701429", "201736990", "201739300", "201700757",
      "201718929", "201702952", "201700547", "201706806", "201719209",
      "201716330", "201717213", "201729780", "201716854", "201706120",
      "201738468", "201723966", "201728102", "201716295", "201712166",
      "201730530", "201718059", "201738192", "201712339", "201730254",
      "201720218", "201710669", "201716992", "201706016", "201726926",
      "201702298", "201708274", "201733376", "201715815", "201709632",
      "201702368", "201723256", "201707923", "201723074", "201701214",
      "201706497", "201718778", "201725384", "201702957", "201704427",
      "201715846", "201738201", "201705604", "201720205", "201712236",
      "201709700", "201707337", "201717024", "201735168", "201729845",
      "201719024", "201734024", "201708122", "201709756", "201705192",
      "201703514", "201739704", "201725564", "201709754", "201720900",
      "201721829", "201714870", "201716469", "201715494", "201721953",
      "201709449", "201720032", "201727693", "201710603", "201738629",
      "201728080", "201726088", "201734445", "201720099", "201734239",
      "201723303", "201735793", "201725906", "201721716", "201708154",
      "201732471", "201712454", "201723841", "201739867", "201721290",
      "201712282", "201702366", "201700714", "201723399", "201713877")
  expectedBvs <-
    c(6190.76, 2257.61, 1673.08, 5431.64, 3502.81, 822.45, 803.83, 973.33,
      5312.15, 2838.81, 1654.08, 9709.91, 2879.87, 1198.76, 3130.17, 3463.43,
      5954.25, 1286.95, 4040.67, 1512.94, 3994.10, 1070.06, 2209.71, 5760.58,
      354.78, 5644.11, 2834.22, 2646.35, 1243.22, 1728.11, 3772.12, 6109.28,
      2153.29, 1995.51, 5761.85, 1257.95, 2094.59, 808.52, 772.92, 3552.32,
      3475.91, 2506.77, 1132.14, 2916.83, 4279.81, 2701.24, 1401.60, 1069.63,
      213.12, 3699.70, 6041.91, 3374.79, 4097.29, 2327.97, 272.42, 496.48,
      1806.69, 4087.45, 11586.13, 3670.93, 180.94, 4240.38, 4816.08, 2337.81,
      2069.89, 438.86, 2718.89, 3633.69, 3899.92, 2464.85, 1266.72, 2053.13,
      2226.06, 953.92, 1047.50, 4879.11, 4234.20, 2758.53, 5601.59, 1755.83,
      1100.42, 1441.32, 3350.71, 1831.84, 5087.97, 1222.36, 6123.77, 286.49,
      3326.18, 5905.08, 189.49, 874.61, 602.69, 469.37, 834.14, 2491.39,
      1275.79, 668.19, 1463.59, 3465.43, 1473.58, 2123.28, 9566.91, 3892.49,
      1060.57, 3859.18, 584.75, 1040.36, 4880.36, 3776.82, 2853.17, 5178.95,
      3936.33, 1873.51, 2710.66, 1888.72, 1986.57, 2629.08, 3630.04, 1737.59,
      1888.35, 3241.33, 3132.98, 3236.81, 437.70, 1749.25, 2144.56, 293.61,
      2010.53, 1505.02, 1191.79, 3942.46, 1019.47, 1010.55, 2108.92, 1851.25,
      945.74, 6497.08, 1037.12, 375.36, 8563.98, 4435.31, 1109.87, 5103.70,
      7572.53)
  checkSample <- c()
  checkMonetary <- c()
  for (i in seq_len(length(itemNumbers))) {

    checkSample[i] <- ifelse(itemNumbers[i] == expectedItems[i], TRUE, FALSE)
    checkMonetary[i] <- ifelse(bookValues[i] == expectedBvs[i], TRUE, FALSE)
  }
  expect_true(unique(checkSample) == TRUE)
  expect_true(unique(checkMonetary) == TRUE)
})

ar <- mus_obj(bv = accounts_receivable$amount,
              id = accounts_receivable$invoice,
              cl = 0.95,
              pm = 450000,
              ee = 100000)

ar <- size(ar, evalMeth = "stringer")

ar <- select(ar,
             selMeth = "randomized.fixed",
             seed = 12345)

audit_values <- accounts_receivable[match(ar$sample$item,
                                          accounts_receivable$invoice),
                                    c("invoice", "av1")]

test_that("audit values are correctly extracted from test file", {

  invoices <- audit_values$invoice
  amounts  <- audit_values$av1

  expectedInvoices <-
    c(201711151, 201720628, 201726641, 201727494, 201711802,
      201703009, 201703086, 201703001, 201701092, 201711888)
  expectedAmounts <- c(6190.76, 2257.61, 1673.08,  5431.64, 3502.81,
                       822.45,  803.83,    0.00,  5312.15, 2838.81)
  checkSample <- c()
  checkMonetary <- c()
  for (i in 1:10) {
    checkSample[i] <- ifelse(invoices[i] == expectedInvoices[i], TRUE, FALSE)
    checkMonetary[i] <- ifelse(amounts[i] == expectedAmounts[i], TRUE, FALSE)
  }
  expect_true(unique(checkSample) == TRUE)
  expect_true(unique(checkMonetary) == TRUE)
})

test_that("MUS cell evaluation match", {
  ar <- select(ar,
               selMeth = "randomized.fixed",
               seed = 345,
               pm = 450000,
               ee = 100000)
  selection <-  mus_select(id = accounts_receivable$invoice,
                           bv = accounts_receivable$amount,
                           n = 145,
                           selMeth = "randomized.fixed",
                           seed = 345,
                           pm = 450000,
                           ee = 100000,
                           dist = "hyper",
                           evalMeth = "stringer")
  sample <- selection[[1]]

  audit_values <- accounts_receivable[match(ar$sample$item,
                                            accounts_receivable$invoice),
                                      c("invoice", "av2")]

  testResults <- mus_eval(cl = 0.95,
                          bv = sample$bv,
                          av = audit_values$av2,
                          popBv = 13500000)
  expect_equal(testResults$Over$`Precision calculation`$stageUPL[1], 276050)
  expect_equal(testResults$Over$`Projected misstatement`, 115448.28,
               tolerance = 1e-06)
  expect_equal(testResults$Over$`Precision achieved`, 342903.55,
               tolerance = 1e-06)
  expect_equal(testResults$Over$`Upper confidence bound`, 458351.83,
               tolerance = 1e-06)

})
