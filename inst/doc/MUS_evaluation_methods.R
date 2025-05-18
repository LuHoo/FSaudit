## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo=TRUE---------------------------------------------------------------
library(FSaudit)

## -----------------------------------------------------------------------------
mySample <- mus_obj(bv = inventoryData$bv,
                    id = inventoryData$item,
                    pm = 1000000,
                    ee = 100000,
                    seed = 98765,
                    evalMeth = "cell")

## -----------------------------------------------------------------------------
mySample <- size(mySample)
mySample$n

## -----------------------------------------------------------------------------
mySample <- select(mySample)
head(mySample$sample)

## -----------------------------------------------------------------------------
audit_values <- inventoryData[match(mySample$sample$item, inventoryData$item), c("item", "av")]
head(audit_values)

## ---- echo=TRUE---------------------------------------------------------------
mySample <- evaluate(mySample,
                     av = audit_values$av)
mySample$evalResults$Over$`Nonzero differences`
mySample$evalResults$Over$`Projected misstatement`
mySample$evalResults$Over$`Basic precision`
mySample$evalResults$Over$`Precision achieved`
mySample$evalResults$Over$`Upper confidence bound`

## ---- echo=TRUE---------------------------------------------------------------

mySample$evalResults$Under$`Nonzero differences`
mySample$evalResults$Under$`Projected misstatement`
mySample$evalResults$Under$`Basic precision`
mySample$evalResults$Under$`Precision achieved`
mySample$evalResults$Under$`Upper confidence bound`

## -----------------------------------------------------------------------------
mySample$evalResults$Over$`Precision calculation`

## -----------------------------------------------------------------------------
mySample$evalResults$Under$`Precision calculation`

## ---- echo=TRUE---------------------------------------------------------------
mySampleStringer <- evaluate(mySample,
                             av = audit_values$av,
                             evalMeth = "stringer")

## ---- echo=TRUE---------------------------------------------------------------
mySampleStringer$evalResults$Over$`Nonzero differences`
mySampleStringer$evalResults$Over$`Projected misstatement`
mySampleStringer$evalResults$Over$`Basic precision`
mySampleStringer$evalResults$Over$`Precision achieved`
mySampleStringer$evalResults$Over$`Upper confidence bound`

## -----------------------------------------------------------------------------
mySampleStringer$evalResults$Under$`Nonzero differences`
mySampleStringer$evalResults$Under$`Projected misstatement`
mySampleStringer$evalResults$Under$`Basic precision`
mySampleStringer$evalResults$Under$`Precision achieved`
mySampleStringer$evalResults$Under$`Upper confidence bound`

## -----------------------------------------------------------------------------
mySampleStringer$evalResults$Over$`Precision calculation`

## -----------------------------------------------------------------------------
mySampleStringer$evalResults$Under$`Precision calculation`


## -----------------------------------------------------------------------------
mySamplePPS <- evaluate(mySample,
                        av = audit_values$av,
                        evalMeth = "pps")

## -----------------------------------------------------------------------------
mySamplePPS$evalResults$`Nonzero diff`
mySamplePPS$evalResults$`pps estimate`
mySamplePPS$evalResults$`Error estimate`
mySamplePPS$evalResults$Precision
mySamplePPS$evalResults$`Effective df`
mySamplePPS$evalResults$`Lower bound`
mySamplePPS$evalResults$`Upper bound`

## -----------------------------------------------------------------------------
mySample$evalResults$Over$`Projected misstatement` - 
  mySample$evalResults$Under$`Projected misstatement`
mySampleStringer$evalResults$Over$`Projected misstatement` - 
  mySampleStringer$evalResults$Under$`Projected misstatement`
mySamplePPS$evalResults$`Error estimate`

## -----------------------------------------------------------------------------
mySample$evalResults$Over$`Precision achieved`
mySample$evalResults$Under$`Precision achieved`
mySampleStringer$evalResults$Over$`Precision achieved`
mySampleStringer$evalResults$Under$`Precision achieved`
mySamplePPS$evalResults$Precision

