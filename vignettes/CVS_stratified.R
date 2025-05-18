## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
RNGkind(sample.kind = "Rounding")
library(FSaudit)
myStrat <- cvs_obj(bv = inventoryData$bv, id = inventoryData$item)

## -----------------------------------------------------------------------------
myStrat <- stratify(myStrat, stratMeth = "equal", strata = 3)
myStrat$stratSumm

## -----------------------------------------------------------------------------

myStrat <- stratify(myStrat, stratMeth = "cumulative", strata = 3, classes = 10)

## -----------------------------------------------------------------------------
myStrat$classSumm

## -----------------------------------------------------------------------------
myStrat$stratSumm
myStrat$alloc

## -----------------------------------------------------------------------------
myStrat <- stratify(myStrat, stratMeth = "cumulative", strata = 3, classes = 10, n = 400)
myStrat$alloc

## -----------------------------------------------------------------------------
myStrat <- size(myStrat, desPrec = 200000)
myStrat$n
myStrat$alloc

## -----------------------------------------------------------------------------
sum(myStrat$alloc$nh)

## -----------------------------------------------------------------------------
myStrat <- select(myStrat, seed = 12345)
head(myStrat$sample)

## -----------------------------------------------------------------------------
audit_values <- inventoryData[match(myStrat$sample$item,
                                   inventoryData$item),
                             c("item", "av")]

## -----------------------------------------------------------------------------
myStrat <- evaluate(myStrat, av = audit_values$av)

## -----------------------------------------------------------------------------
myStrat$evalResults$Estimates


## -----------------------------------------------------------------------------
myStrat$evalResults$`Mean per unit estimation`

## -----------------------------------------------------------------------------
myStrat$evalResults$`Regression estimation`

