## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(FSaudit)
ar <- mus_obj(bv = accounts_receivable$amount,
              id = accounts_receivable$invoice)
ar$popn
ar$popBv

## -----------------------------------------------------------------------------
ar <- size(ar, 
           pm = 450000,
           ee = 100000)
ar$n

## -----------------------------------------------------------------------------
alternative <- size(ar, dist = "pois")
alternative$n

## -----------------------------------------------------------------------------
RNGkind(sample.kind = "Rounding")
ar <- select(ar,
             selMeth = "randomized.fixed",
             seed = 345)

## -----------------------------------------------------------------------------
head(ar$sample)

## -----------------------------------------------------------------------------
av <- ar$sample$bv

## -----------------------------------------------------------------------------
av[16]
av[52]
av[124]
av[16] <- 4438.82
av[52] <- 0.00
av[124] <- 5531.38
av[16]
av[52]
av[124]

## -----------------------------------------------------------------------------
ar <- evaluate(ar,
               av = av)

## -----------------------------------------------------------------------------
ar$evalResults$Over$`Projected misstatement`
ar$evalResults$Over$`Sample size used`
ar$evalResults$Over$`Nonzero differences`
ar$evalResults$Over$`Cell width`
ar$evalResults$Over$`Precision achieved`
ar$evalResults$Over$`Upper confidence bound`

## -----------------------------------------------------------------------------
ar$evalResults$Under

## -----------------------------------------------------------------------------
ar$evalResults$Over$`Precision calculation`

## -----------------------------------------------------------------------------
(errors <- which(ar$sample$bv != ar$av))

## -----------------------------------------------------------------------------
misstatements <- data.frame(id = ar$sample$item[errors],
bv = ar$sample$bv[errors],
av = ar$av[errors])
misstatements$diff <- misstatements$bv - misstatements$av
misstatements$taint <- misstatements$diff / misstatements$bv
misstatements

## -----------------------------------------------------------------------------
ar <- evaluate(ar, 
               av = av,
               evalMeth = "stringer")

## -----------------------------------------------------------------------------
ar$evalResults$Over$`Projected misstatement`
ar$evalResults$Over$`Sample size used`
ar$evalResults$Over$`Nonzero differences`
ar$evalResults$Over$`Sampling interval`
ar$evalResults$Over$`Precision achieved`
ar$evalResults$Over$`Upper confidence bound`

## -----------------------------------------------------------------------------
ar$evalResults$Over$`Precision calculation`

## -----------------------------------------------------------------------------
ar <- evaluate(ar,
               av = av,
               evalMeth = "pps")
ar$evalResults$'Nonzero diff'
ar$evalResults$'pps estimate'
ar$evalResults$'Error estimate'
ar$evalResults$Precision
ar$evalResults$'Effective df'
ar$evalResults$'Lower bound'
ar$evalResults$'Upper bound'

