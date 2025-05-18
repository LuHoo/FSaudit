library(testthat)
library(FSaudit)
library(tibble)

# MUS workflow

# 1. Split population

split <- split_population(inventoryData$item,
                          inventoryData$bv)

# 2a. Create mus_obj with performance materiality

myMusObject <- mus_obj(id = split$pos_bv$item,
                       bv = split$pos_bv$bv,
                       pm = 100000)

test_that("Seed is not NULL when mus_obj is created with bv", {
  expect_type(myMusObject$seed, "double")
})

# 2b. Create mus_obj with seed

myMusObject <- mus_obj(id = split$pos_bv$item,
                       bv = split$pos_bv$bv,
                       seed = 18061960)

mySeed <- 18061960

test_that("Seed is user defined", {
  expect_equal(myMusObject$seed, mySeed)
})


# 3. Calculate sample size

myMusObject <- size(myMusObject, pm = 1000000, evalMeth = "stringer")

test_that("Seed is not changed when perfMat is changed later", {
  expect_equal(myMusObject$seed, mySeed)
})


# CVS workflow

# 1a. Create cvs_obj with desired precision.

myCvsObject <- cvs_obj(id = inventoryData$item,
                       bv = inventoryData$bv,
                       desPrec = 1000000)

test_that("Seed is not NULL when cvs_obj is created with bv", {
  expect_type(myCvsObject$seed, "double")
})

# 1c. Create cvs_obj with specific seed.

myCvsObject <- cvs_obj(id = inventoryData$item,
                       bv = inventoryData$bv,
                       seed = 2323)
mySeed2 <- 2323

test_that("Seed is user defined", {
  expect_equal(myCvsObject$seed, mySeed2)})

myCvsObject <- size(myCvsObject, desPrec = 200000)

test_that("Seed is not changed when perfMat is changed later", {
  expect_equal(myCvsObject$seed, mySeed2)
})

# 2. Stratify the population

myCvsObject <- stratify(myCvsObject,
                        strata = 3,
                        classes = 50,
                        stratMeth = "equal")

test_that("Seed is still the same", {
  expect_equal(myCvsObject$seed, mySeed2)
})

# 3. Calculate sample size

myCvsObject <- size(myCvsObject,
                    desPrec = 200000)

test_that("Seed is still the same", {
  expect_equal(myCvsObject$seed, mySeed2)
})
