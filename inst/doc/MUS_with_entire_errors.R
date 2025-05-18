## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(FSaudit)
subsidies <- mus_obj(cl = 0.95, 
                     popBv = 12000000, 
                     pm = 120000)

## -----------------------------------------------------------------------------
subsidies <- size(subsidies)
subsidies$n

## -----------------------------------------------------------------------------
myAttSample <- att_obj(alpha = 0.05, popn = 12000000, popdev = 120000)
myAttSample <- size(myAttSample, c = 0)
myAttSample$n

## -----------------------------------------------------------------------------
subsidies <- size(subsidies,
                  ee = 25369)
subsidies$n

## -----------------------------------------------------------------------------
(expectedError <- subsidies$popBv / 473)

## -----------------------------------------------------------------------------
myAttSample <- size(myAttSample, c = 1)
myAttSample$n

## -----------------------------------------------------------------------------
myAttSample <- size(myAttSample, ee = 20000)
myAttSample$n
myAttSample$c

## -----------------------------------------------------------------------------
subsidies <- size(subsidies,
                  ee = 20000)
subsidies$n

## -----------------------------------------------------------------------------
myAttSample <- att_obj(alpha = 0.05, popn = 12000000, popdev = 120000)
myAttSample <- size(myAttSample, c = 1)
myAttSample$n
subsidies <- mus_obj(cl = 0.95, 
                     popBv = 12000000, 
                     pm = 120000,
                     n = myAttSample$n)
subsidies$n

