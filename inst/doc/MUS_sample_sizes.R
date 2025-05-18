## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo=TRUE---------------------------------------------------------------
library(FSaudit)

## -----------------------------------------------------------------------------
mySample <- mus_obj(bv =  accounts_receivable$amount,
                    id = accounts_receivable$invoice,
                    pm = 450000,
                    ee = 100000,
                    evalMeth = "stringer")

## -----------------------------------------------------------------------------
mySample <- size(mySample)
mySample$n

## -----------------------------------------------------------------------------
populationSize <- length(accounts_receivable$amount)
populationSize

## ---- echo = TRUE-------------------------------------------------------------
mySampleBinomial <- size(mySample, dist = "binom")
mySampleBinomial$n

## -----------------------------------------------------------------------------
populationErrorRate <- mySample$pm / mySample$popBv
populationErrorRate

## ---- echo = TRUE-------------------------------------------------------------
mySamplePoisson <- size(mySample, dist = "pois")
mySamplePoisson$n

