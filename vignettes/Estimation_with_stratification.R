## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(FSaudit)
cvs_procedure <- cvs_obj()

## -----------------------------------------------------------------------------
names(cvs_procedure)

## -----------------------------------------------------------------------------
head(inventoryData)
summary(inventoryData)

## -----------------------------------------------------------------------------
cvs_procedure$confidenceLevel

## -----------------------------------------------------------------------------
cvs_stratified_experiment <- cvs_obj(bookValues = inventoryData$bv, samplingUnits = inventoryData$item)

## -----------------------------------------------------------------------------
cvs_stratified_experiment <- stratifySample(cvs_stratified_experiment, L = 3, classes = 10, stratificationMethod = "cumulative")

## -----------------------------------------------------------------------------
cvs_stratified_experiment$stratificationClasses

## -----------------------------------------------------------------------------
df <- data.frame(bv = cvs_stratified_experiment$bookValues, su = cvs_stratified_experiment$samplingUnits, lbl = cvs_stratified_experiment$stratumLabel)
head(df)

## -----------------------------------------------------------------------------
cvs_stratified_experiment <- getSampleSize(cvs_stratified_experiment, desiredPrecision = 500000)

## -----------------------------------------------------------------------------
cvs_stratified_experiment$n

## -----------------------------------------------------------------------------
cvs_stratified_experiment <- select(cvs_stratified_experiment)

## -----------------------------------------------------------------------------
head(cvs_stratified_experiment$sample)

## -----------------------------------------------------------------------------
audit_values <- inventoryData[match(cvs_stratified_experiment$sample$item, inventoryData$item), "av"]

length(audit_values)

## -----------------------------------------------------------------------------
cvs_stratified_experiment <- evaluate(cvs_stratified_experiment, auditValues = audit_values)

