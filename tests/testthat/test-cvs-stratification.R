library(testthat)
library(FSaudit)

context("CVS stratification")

# Table 4.2: Stratification with equal boundaries

test_that("Warning for RNGkind", {
  expect_warning(RNGkind(sample.kind = "Rounding"))
})

test_that("User is warned when number of strata too large", {
  myCvsObject <- cvs_obj(id = inventoryData$item, bv = inventoryData$bv)
  expect_error(myCvsObject <- stratify(myCvsObject, strata = 1751))
})

test_that("User is warned when number of strata too small", {
  myCvsObject <- cvs_obj(id = inventoryData$item, bv = inventoryData$bv)
  expect_error(myCvsObject <- stratify(myCvsObject, strata = 1))
})


test_that("Equal method boundaries match", {
  invStSamp <- cvs_obj(bv = inventoryData$bv,
                       id = inventoryData$item)

  invStSampEq <- stratify(invStSamp,
                          strata = 3,
                          stratMeth = "equal")

  equal_3 <- data.frame(strat = c(1, 2, 3),
                        cum_bv = c(2452927.31, 2453454.96, 2454433.73),
                        freq   = c(2431, 710, 359),
                        minBv = c(1.57, 2480.29, 4839.62),
                        maxBv = c(2478.93, 4833.28, 18496.21))

  equal_3$strat <- as.factor(equal_3$strat)

  expect_true(all.equal(invStSampEq$stratSumm, equal_3))

})

# Table 4.3: Classification of Inventories

test_that("Classification for cumulative method matches", {
  invStSamp <- cvs_obj(bv = inventoryData$bv,
                       id = inventoryData$item)
  invStSamp <- stratify(invStSamp,
                        strata = 3,
                        classes = 10,
                        stratMeth = "cumulative")

  classes_10 <-
    data.frame(minBv = c(1.57, 1851.65, 3700.70, 5556.67, 7406.10, 9273.30,
                          11299.53, 13131.76, 18496.21),
               maxBv = c(1848.60, 3694.56, 5547.71, 7398.87, 9238.08, 11077.31,
                          12723.03, 14125.05, 18496.21),
               freq   = c(2030, 857, 360, 154, 63, 24, 7, 4, 1),
               sqrtFreq = c(45.055521, 29.274562, 18.973666, 12.409674,
                            7.937254, 4.898979, 2.645751, 2.00, 1),
               cumSum = c(45.055521, 74.33008, 93.30375, 105.71342, 113.65068,
                          118.54966, 121.19541, 123.19541, 124.19541))

  expect_equal(invStSamp$classSumm$minBv, classes_10$minBv)
  expect_equal(invStSamp$classSumm$maxBv, classes_10$maxBv)
  expect_equal(invStSamp$classSumm$freq, classes_10$freq)
  expect_equal(invStSamp$classSumm$sqrtFreq, classes_10$sqrtFreq,
               tolerance = 1e-05)
  expect_equal(invStSamp$classSumm$cmSum, classes_10$cumSum,
               tolerance = 1e-05)

})

# Table 4.4: Stratification with cumulative square root of frequencies


test_that("Cumulative method boundaries match", {
  invStSamp <- cvs_obj(bv = inventoryData$bv,
                       id = inventoryData$item)
  invStSamp <- stratify(invStSamp,
                        strata = 3,
                        classes = 10,
                        stratMeth = "cumulative")

  cumulative_3 <- data.frame(strat = c(1, 2, 3),
                        minBv = c(1.57, 1851.65, 5556.67),
                        maxBv = c(1848.60, 5547.71, 18496.21),
                        freq   = c(2030, 1217, 253),
                        cumSum = c(45.05552, 48.24823, 30.89166))

  expect_equal(invStSamp$stratSumm$strat, cumulative_3$strat)
  expect_equal(invStSamp$stratSumm$minBv, cumulative_3$minBv)
  expect_equal(invStSamp$stratSumm$maxBv, cumulative_3$maxBv)
  expect_equal(invStSamp$stratSumm$freq, cumulative_3$freq)
  expect_equal(invStSamp$stratSumm$cumSum, cumulative_3$cumSum,
               tolerance = 1e-06)

})

test_that("Allocation of preassigned sample size matches", {
  invStSamp <- cvs_obj(bv = inventoryData$bv,
                       id = inventoryData$item)

  invStSampEq <- stratify(invStSamp,
                          strata = 3,
                          stratMeth = "equal",
                          n = 300)

  expect_equal(invStSampEq$alloc$nh[1], 100)
  expect_equal(invStSampEq$alloc$nh[2], 100)
  expect_equal(invStSampEq$alloc$nh[3], 100)
})

#========================================================
# Testing function cvs_size                             #
#========================================================

# Table 4.4: Sample size calculation for stratified sample

test_that("Sample size for each strata match using proportional allocation", {
  invStSamp <- cvs_obj(bv = inventoryData$bv,
                       id = inventoryData$item)
  invStSamp <- stratify(invStSamp,
                        strata = 3,
                        classes = 10,
                        stratMeth = "cumulative")
  invStSamp <- size(invStSamp,
                    desPrec = 200000)

  table_4_5 <- data.frame(strat = c(1, 2, 3),
                          Sh = c(524.12, 1001.24, 1828.32),
                          popNh = c(2030, 1217, 253),
                          popBvh = c(1590003, 3865204, 1905608),
                          nh = c(226, 259, 98))

  expect_equal(invStSamp$alloc$strat, as.factor(table_4_5$strat))
  expect_equal(invStSamp$alloc$popAvh, table_4_5$popAvh, tolerance = 1e-06)
  expect_equal(invStSamp$alloc$popNh, table_4_5$popNh)
  expect_equal(invStSamp$alloc$popBvh, table_4_5$popBvh, tolerance = 1e-06)
  expect_equal(invStSamp$alloc$nh, table_4_5$nh)

})

#============================================
# Testing sample size calculation function  #
#============================================

test_that("Unstratified -- Sample size given", {
  case1 <- cvs_obj(n = 10,
                   bv = salaries$gross,
                   id = salaries$id,
                   seed = 12345)
  case1 <- select(case1)
  case1$sample

  expect_equal(case1$alloc$nh, 10)
  expect_equal(case1$sample$item[1], 910046)

})

test_that("Unstratified -- sdav given", {
  case2 <- cvs_obj(desPrec = 300000,
                   bv = salaries$gross,
                   id = salaries$id,
                   seed = 12345,
                   sdAv = 1537.55)
  case2 <- size(case2)
  case2$n

  expect_equal(case2$n, 411)
})

test_that("Unstratified -- sdav not given", {
  case3 <- cvs_obj(desPrec = 300000,
                   bv = salaries$gross,
                   id = salaries$id,
                   seed = 12345)
  case3 <- size(case3)
  case3$n

  expect_equal(case3$n, 504)

})

test_that("Unstratified -- recalc df", {
  case4 <- cvs_obj(desPrec = 600000,
                   bv = salaries$gross,
                   id = salaries$id,
                   seed = 12345)
  case4 <- size(case4)
  case4$n

  expect_equal(case4$n, 153)

})

test_that("Stratified -- sd_av given", {
  invStSamp <- cvs_obj(bv = inventoryData$bv,
                       id = inventoryData$item)
  invStSamp <- stratify(invStSamp,
                        strata = 3,
                        classes = 10,
                        stratMeth = "cumulative")

  case6 <- size(invStSamp,
                desPrec = 200000,
                sdAv = c(575, 1100, 2000))
  case6$n

  expect_equal(case6$alloc$nh[1], 226)
  expect_equal(case6$alloc$nh[2], 259)
  expect_equal(case6$alloc$nh[3], 98)

})

test_that("Stratified -- sd_av not given", {

  invStSamp <- cvs_obj(bv = inventoryData$bv,
                       id = inventoryData$item)

  invStSamp <- stratify(invStSamp,
                        strata = 3,
                        classes = 10,
                        stratMeth = "cumulative")

  case7 <- size(invStSamp, desPrec = 200000)
  case7$n

  expect_equal(case7$alloc$nh[1], 226)
  expect_equal(case7$alloc$nh[2], 259)
  expect_equal(case7$alloc$nh[3], 98)

})

test_that("User is warned when sample size not given or calculated", {
  myCvsObject <- cvs_obj(id = inventoryData$item, bv = inventoryData$bv)
  expect_error(myCvsObject <- select(myCvsObject))
})

#==============================================================================#
# Testing evaluation of stratified samples                                     #
#==============================================================================#

idea <- cvs_obj(id = inventoryData$item,
                bv = inventoryData$bv)
idea <- stratify(idea,
                 strata = 3,
                 classes = 200,
                 n = 361,
                 stratMeth = "cumulative")
idea <- select(idea)
idea$sample$item <- cvsTest$SAM_RECNO
idea$sample$bv   <- cvsTest$BV
idea$sample$item <- cvsTest$SAM_RECNO
idea$sample$bv   <- cvsTest$BV

test_that("Scenario1 -- all differences", {
  true_values <- cvsTest$AV_SCEN1
  idea <- evaluate(idea,
                   av = true_values)
  est <- idea$evalResults$Estimates
  # mpu
  expect_equal(est$mpu[1], 7366741.14)
  expect_equal(est$mpu[2], -5925.14, tolerance = 1e-6)
  expect_equal(est$mpu[3], 289551.46, tolerance = 1e-5)
  expect_equal(idea$evalResults[[2]]$Est_regressor_var,
               c(208771, 771350, 5776192),
               tolerance = 1e-5)
  # dif
  expect_equal(est$diff[1], 7401248.86)
  expect_equal(est$diff[2], -40432.86, tolerance = 1e-6)
  expect_equal(est$diff[3], 157689.18, tolerance = 1e-5)
  expect_equal(idea$evalResults[[6]]$Est_regressor_var,
               c(26043, 269683, 2125519),
               tolerance = 1e-5)
  # ratio
  expect_equal(est$ratio[1], 7401439.3)
  expect_equal(est$ratio[2], -40623.3, tolerance = 1e-6)
  expect_equal(est$ratio[3], 157711.17, tolerance = 1e-5)
  expect_equal(idea$evalResults[[10]]$Est_regressor_var,
               c(26173, 269798, 2123371),
               tolerance = 1e-5)
  # regr
  expect_equal(est$regr[1], 7400953.35)
  expect_equal(est$regr[2], -40137.35, tolerance = 1e-6)
  expect_equal(est$regr[3], 158327, tolerance = 1e-5)
  expect_equal(idea$evalResults[[14]]$Est_regressor_var,
               c(26099, 271831, 2146278),
               tolerance = 1e-5)

})

test_that("Scenario1 -- 21 differences per stratum", {
  true_values <- cvsTest$AV_SCEN2
  idea <- evaluate(idea,
                   av = true_values)
  est <- idea$evalResults$Estimates
  # mpu
  expect_equal(est$mpu[1], 6601879.89)
  expect_equal(est$mpu[2], 758936.11, tolerance = 1e-6)
  expect_equal(est$mpu[3], 358457.63, tolerance = 1e-5)
  expect_equal(idea$evalResults[[2]]$Est_regressor_var,
               c(226670, 1453658, 8623809),
               tolerance = 1e-5)
  # dif
  expect_equal(est$diff[1], 6636387.62)
  expect_equal(est$diff[2], 724428.38, tolerance = 1e-6)
  expect_equal(est$diff[3], 174826.85, tolerance = 1e-4)
  expect_equal(idea$evalResults[[6]]$Est_regressor_var,
               c(2047, 407355, 2622521),
               tolerance = 1e-5)
  # ratio
  expect_equal(est$ratio[1], 6632975.48)
  expect_equal(est$ratio[2], 727840.52, tolerance = 1e-6)
  expect_equal(est$ratio[3], 186833.7, tolerance = 1e-5)
  expect_equal(idea$evalResults[[10]]$Est_regressor_var,
               c(5977, 464506, 2926921),
               tolerance = 1e-5)
  # regr
  expect_equal(est$regr[1], 6647284.02)
  expect_equal(est$regr[2], 713531.98, tolerance = 1e-6)
  expect_equal(est$regr[3], 157269, tolerance = 1e-5)
  expect_equal(idea$evalResults[[14]]$Est_regressor_var,
               c(16506, 295354, 2091726),
               tolerance = 1e-5)
})

test_that("Scenario3 -- 11 differences per stratum", {
  true_values <- cvsTest$AV_SCEN3
  idea <- evaluate(idea,
                   av = true_values)
  est <- idea$evalResults$Estimates
  # mpu
  expect_equal(est$mpu[1], 6964554.99)
  expect_equal(est$mpu[2], 396261.01, tolerance = 1e-6)
  expect_equal(est$mpu[3], 310558.99, tolerance = 1e-5)
  expect_equal(idea$evalResults[[2]]$Est_regressor_var,
               c(211341, 1022673, 6168159),
               tolerance = 1e-5)
  # dif
  expect_equal(est$diff[1], 6999062.71)
  expect_equal(est$diff[2], 361753.29, tolerance = 1e-6)
  expect_equal(est$diff[3], 128570.44, tolerance = 1e-4)
  expect_equal(idea$evalResults[[6]]$Est_regressor_var,
               c(305, 219208, 1443333),
               tolerance = 1e-5)
  # ratio
  expect_equal(est$ratio[1], 6997358.82)
  expect_equal(est$ratio[2], 363457.18, tolerance = 1e-6)
  expect_equal(est$ratio[3], 132897.52, tolerance = 1e-4)
  expect_equal(idea$evalResults[[10]]$Est_regressor_var,
               c(1095, 234486, 1524383),
               tolerance = 1e-5)
  # regr
  expect_equal(est$regr[1], 7004785.27)
  expect_equal(est$regr[2], 356030.73, tolerance = 1e-6)
  expect_equal(est$regr[3], 122418, tolerance = 1e-5)
  expect_equal(idea$evalResults[[14]]$Est_regressor_var,
               c(5018, 188010, 1297182),
               tolerance = 1e-5)
})

test_that("Scenario4 -- 9 differences per stratum", {
  true_values <- cvsTest$AV_SCEN4
  idea <- evaluate(idea,
                   av = true_values)
  est <- idea$evalResults$Estimates
  # mpu
  expect_equal(est$mpu[1], 7033594.66)
  expect_equal(est$mpu[2], 327221.34, tolerance = 1e-6)
  expect_equal(est$mpu[3], 299773.16, tolerance = 1e-5)
  expect_equal(idea$evalResults[[2]]$Est_regressor_var,
               c(209124, 933081, 5652310),
               tolerance = 1e-5)
  # dif
  expect_equal(est$diff[1], 7068102.38)
  expect_equal(est$diff[2], 292713.62, tolerance = 1e-6)
  expect_equal(est$diff[3], 144273.3, tolerance = 1e-4)
  expect_equal(idea$evalResults[[6]]$Est_regressor_var,
               c(238, 280142, 1787948),
               tolerance = 1e-5)
  # ratio
  expect_equal(est$ratio[1], 7066723.67)
  expect_equal(est$ratio[2], 294092.33, tolerance = 1e-6)
  expect_equal(est$ratio[3], 161199.8, tolerance = 1e-4)
  expect_equal(idea$evalResults[[10]]$Est_regressor_var,
               c(1164, 351471, 2200436),
               tolerance = 1e-5)
  # regr
  expect_equal(est$regr[1], 7072777.42)
  expect_equal(est$regr[2], 288038.58, tolerance = 1e-6)
  expect_equal(est$regr[3], 140366, tolerance = 1e-5)
  expect_equal(idea$evalResults[[14]]$Est_regressor_var,
               c(3486, 258065, 1682447),
               tolerance = 1e-5)

})

test_that("Scenario5 -- 4 differences per stratum", {
  true_values <- cvsTest$AV_SCEN5
  idea <- evaluate(idea,
                   av = true_values)
  est <- idea$evalResults$Estimates
  # mpu
  expect_equal(est$mpu[1], 7198732.46)
  expect_equal(est$mpu[2], 162083.54, tolerance = 1e-6)
  expect_equal(est$mpu[3], 270891.67, tolerance = 1e-5)
  expect_equal(idea$evalResults[[2]]$Est_regressor_var,
               c(205934.2, 705962, 4332960),
               tolerance = 1e-5)
  # dif
  expect_equal(est$diff[1], 7233240.18)
  expect_equal(est$diff[2], 127575.82, tolerance = 1e-6)
  expect_equal(est$diff[3], 128074.6, tolerance = 1e-4)
  knownDiff <- c(0.698, 61.641, 154.073)
  expect_equal(idea$evalResults[[6]]$Est_regressor_var,
               c(20.41, 167937, 1066455),
               tolerance = 1e-5)
  # ratio
  expect_equal(est$ratio[1], 7232639.29)
  expect_equal(est$ratio[2], 128176.71, tolerance = 1e-6)
  expect_equal(est$ratio[3], 152456.1, tolerance = 1e-4)
  # regr
  expect_equal(est$regr[1], 7235304.16)
  expect_equal(est$regr[2], 125511.84, tolerance = 1e-6)
  expect_equal(est$regr[3], 126961, tolerance = 1e-5)
})

# Test intermediate results ADA

invStSamp <- cvs_obj(bv = inventoryData$bv,
                     id = inventoryData$item)
invStSamp <- stratify(invStSamp,
                      strata = 3,
                      classes = 10,
                      stratMeth = "cumulative")
invStSamp <- size(invStSamp,
                  desPrec = 200000)

invStSamp <- select(invStSamp,
                    seed = 12345)
head(invStSamp$sample)

true_values <- inventoryData[match(invStSamp$sample$item,
                                   inventoryData$item),
                             c("item", "av")]

invStSamp <- evaluate(invStSamp,
                      av = true_values$av)

sample <- data.frame(bv = invStSamp$sample$bv,
                     av = invStSamp$av,
                     strat = invStSamp$sample$strat)

test_that("Intermediate results of the three strata", {
  popNh <- invStSamp$stratSumm$freq
  nh <- invStSamp$alloc$nh
  sum_av <- aggregate(av ~ as.factor(strat), data = sample, FUN =  "sum")
  var_regr <- aggregate(av ~ as.factor(strat), data = sample, FUN = "var")
  expect_equal(popNh, c(2030, 1217, 253))
  expect_equal(nh, c(226, 259, 98))
  expect_equal(sum_av$av, c(183746.92, 853049.17, 779308.12))
  expect_equal(var_regr$av, c(297308.35, 1696612, 7979066.216))

})
