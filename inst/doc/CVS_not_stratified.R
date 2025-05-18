## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(FSaudit)
RNGkind(sample.kind = "Rounding")
(pop_N <- nrow(inventoryData))

## -----------------------------------------------------------------------------
head(inventoryData)

## -----------------------------------------------------------------------------
mySample <- cvs_obj()
mySample$n

## -----------------------------------------------------------------------------
mySample <- cvs_obj(n = 400, 
                    bv = inventoryData$bv, 
                    id = inventoryData$item,
                    seed = 12345)
mySample$n

## -----------------------------------------------------------------------------
mySample <-  select(mySample)

## -----------------------------------------------------------------------------
head(mySample$sample)

## -----------------------------------------------------------------------------
audit_values <- 
  inventoryData[match(mySample$sample$item, inventoryData$item), "av"]

## -----------------------------------------------------------------------------
mySample <- evaluate(mySample, av = audit_values)

## -----------------------------------------------------------------------------
mySample$evalResults$Estimates

## -----------------------------------------------------------------------------
mySample$evalResults$`Mean per unit estimation`
mySample$evalResults$`Most likely total audited amount mean`
mySample$evalResults$`Most likely total error mean`
mySample$evalResults$`Achieved precision mean`

## -----------------------------------------------------------------------------
mySample$evalResults$`Regression estimation`
mySample$evalResults$`Most likely total audited amount regression`
mySample$evalResults$`Most likely total error regression`
mySample$evalResults$`Achieved precision regression`

## -----------------------------------------------------------------------------
mySample$evalResults$`Difference estimation`
mySample$evalResults$`Most likely total audited amount difference`
mySample$evalResults$`Most likely total error difference`
mySample$evalResults$`Achieved precision difference`

## -----------------------------------------------------------------------------
mySample$evalResults$`Ratio estimation`
mySample$evalResults$`Most likely total audited amount ratio`
mySample$evalResults$`Most likely total error ratio`
mySample$evalResults$`Achieved precision ratio`

## -----------------------------------------------------------------------------
secondSample <- cvs_obj(cl = 0.95,
                        desPrec = 400000,
                        bv = inventoryData$bv,
                        id = inventoryData$item,seed = 12345)

## -----------------------------------------------------------------------------
secondSample <- size(secondSample)
secondSample$n

