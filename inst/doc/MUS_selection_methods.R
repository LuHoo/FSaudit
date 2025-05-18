## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo=TRUE---------------------------------------------------------------
library(FSaudit)

## -----------------------------------------------------------------------------
mySample <- mus_obj(cl = 0.95,
                    bv = accounts_receivable$amount,
                    id = accounts_receivable$invoice,
                    pm = 23950,
                    seed = 3)
mySample <- size(mySample)
mySample$n

## -----------------------------------------------------------------------------
sum(accounts_receivable$amount)
samplingInterval <- sum(accounts_receivable$amount) / mySample$n
samplingInterval

## -----------------------------------------------------------------------------
significants <- accounts_receivable[accounts_receivable$amount > samplingInterval, 1:3]
significants[order(-significants$amount),]

## -----------------------------------------------------------------------------
mySample <- select(mySample,
                   selMeth = "random",
                   aggregates = TRUE)
head(mySample$sample[order(-mySample$sample$bv),], 10)

## -----------------------------------------------------------------------------
mySample <- select(mySample,
                   selMeth = "fixed",
                   aggregates = TRUE)
head(mySample$sample[order(-mySample$sample$bv),], 10)

## -----------------------------------------------------------------------------
mySample <- select(mySample,
                   selMeth = "randomized.fixed",
                   aggregates = TRUE)
head(mySample$sample[order(-mySample$sample$bv),], 10)

## -----------------------------------------------------------------------------
mySample <- select(mySample,
                   selMeth = "cell",
                   aggregates = TRUE)
head(mySample$sample[order(-mySample$sample$bv),], 10)

## -----------------------------------------------------------------------------
orderedSample <- mySample$sample[order(-mySample$sample$bv),]
orderedSample[34:35,]

## -----------------------------------------------------------------------------
mySample <- select(mySample,
                   selMeth = "sieve",
                   aggregates = TRUE)
head(mySample$sample[order(-mySample$sample$bv),], 10)

## -----------------------------------------------------------------------------
mySample <- select(mySample,
                   selMeth = "modified.sieve",
                   aggregates = TRUE)
head(mySample$sample[order(-mySample$sample$bv),], 10)

