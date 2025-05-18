## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(FSaudit)
mySample <- mus_obj(cl = 0.95,
                    bv = accounts_receivable$amount,
                    id = accounts_receivable$invoice,
                    pm = 23950)
mySample <- size(mySample)

## -----------------------------------------------------------------------------
mySample$n
mySample$popBv
samplingInterval <- mySample$popBv / mySample$n
samplingInterval

## -----------------------------------------------------------------------------
mySample <- select(mySample,
                   selMeth = "fixed",
                   aggregates = TRUE,
                   seed = 1)
mySample$signItems

## -----------------------------------------------------------------------------
length(mySample$sample$bv)
mySample$n

## -----------------------------------------------------------------------------
mySample2 <- mus_obj(cl = 0.95,
                     bv = accounts_receivable$amount,
                     id = accounts_receivable$invoice,
                     aggregates = FALSE,
                     pm = 23950)
mySample2 <- size(mySample2)
mySample2$n
mySample2$signItems

## -----------------------------------------------------------------------------
mySample2 <- significant(mySample2)
mySample2$signItems

## -----------------------------------------------------------------------------
mySample2$n

## -----------------------------------------------------------------------------
mySample2 <- select(mySample2,
                    selMeth = "fixed",
                    seed = 1)
length(mySample2$sample$bv)

