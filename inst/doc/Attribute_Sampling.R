## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ---- echo=TRUE---------------------------------------------------------------
library(FSaudit)
mySample <- att_obj(alpha = .1,
                    popdev = 60,
                    popn = 1200,
                    c = 0)

## -----------------------------------------------------------------------------
mySample <- size(mySample)
mySample$n

## -----------------------------------------------------------------------------
mySample <- size(mySample, c = 1)
mySample$n

mySample <- size(mySample, c = 2)
mySample$n

## -----------------------------------------------------------------------------
myBinomialSample <- att_obj(alpha = .1,
                            tdr = .05,
                            dist = "binom",
                            c = 2)
myBinomialSample <- size(myBinomialSample)
myBinomialSample$n

## -----------------------------------------------------------------------------
myPoissonSample <- att_obj(alpha = .1,
                           tdr = .05,
                           dist = "pois",
                           c = 2)
myPoissonSample <- size(myPoissonSample)
myPoissonSample$n

## -----------------------------------------------------------------------------
mySample2 <- att_obj(alpha = .1,
                    popdev = 60,
                    popn = 1200,
                    ee = 10)
mySample2 <- size(mySample2)
mySample2$n
mySample2$c

## -----------------------------------------------------------------------------
phyper(q = 1, m = 60, n = 1140, k = 45)

## -----------------------------------------------------------------------------
1 - phyper(0, m = 24, n = 1176, k = 45)
phyper(0, m = 24, n = 1176, k = 45, lower.tail = FALSE)

## -----------------------------------------------------------------------------
phyper(q = 2, m = 60, n = 1140, k = 102)

## -----------------------------------------------------------------------------
upper(popn = 1200, n = 102, k = 0, alpha = 0.10)
upper(popn = 1200, n = 102, k = 1, alpha = 0.10)
upper(popn = 1200, n = 102, k = 2, alpha = 0.10)
upper(popn = 1200, n = 102, k = 3, alpha = 0.10)

## -----------------------------------------------------------------------------
upper(popn = 1200, n = 102, k = 0, alpha = 0.10) / 1200
upper(popn = 1200, n = 102, k = 1, alpha = 0.10) / 1200
upper(popn = 1200, n = 102, k = 2, alpha = 0.10) / 1200
upper(popn = 1200, n = 102, k = 3, alpha = 0.10) / 1200

