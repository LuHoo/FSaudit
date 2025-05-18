#' Accounts receivable
#'
#' A dataset containing 10,000 invoices outstanding per year-end and their
#' amount. Two audit values have also been provided.
#'
#' @format A tibble with 7 variables in the following order:
#' @field debtor Unique ID of the debtor.
#' @field invoice Unique invoice number.
#' @field amount Amount of the invoice.
#' @field av1 Audit value 1.
#' @field av2 Audit value 2.
#' @field diff1 Difference between amount and audit value 1.
#' @field diff2 difference between amount and audit value 2.
#'
"accounts_receivable"

#' Error patterns
#'
#' A dataset consisting of 10,000 elements, with unique item number, book value,
#' and six different error seedings. All seeded errors amount to a 100,000
#' overstatement.
#'
#' @format A data frame with 8 variables in the following order:
#' @field item Item number. Unique identifier ranging from 1 to 10,000.
#' @field bv Book value. Book value of the item. Book values are normally
#'   distributed, with a mean of zero, and range from -500 to 500.
#' @field constdiff Constant difference. Audit values of the item are such that
#'   25\% of them have an overstatement of 40.
#' @field rarediff Rare difference. Like constant difference, but now only 5\%
#'   of the items are incorrect by 200.
#' @field diffslope Difference and slope. Average difference between book and
#'   audit value of 10. Audit values are fractionally off the book values, the
#'   fraction being normally distributed.
#' @field slope Slope only. The audit value is 90\% of the book value, plus a
#'   normally distributed error with mean 0 and sd equal to three times the book
#'   value of the item. Errors are therefore heteroskedastic.
#' @field missing Missing. There are 914 100\% differences, representing missing
#'   elements. All other items are correct.
#' @field scatter Scattered errors. No correlation between the book values and
#'   the audit values.
"errorPatterns"

#' Inventory data
#'
#' A dataset containing the population ID, book values, and audit values with
#' different error distributions of 3,500 inventory items.
#'
#' @format A data frame with 7 variables in the following order:
#' @field item Unique identifier.
#' @field bv Book value.
#' @field av Audit value. Used in Audit Data Analysis case study.
#' @field av_mus Audit value with sparse errors. There are 116 100\%
#' differences, representing missing elements. All other items are correct.
#' @field av_reg Audit value with many errors. Differences between book and
#' audit value are best estimated by the regression estimator (slope and
#' intercept).
#' @field av_dif Audit value with many errors. Differences between book and
#' audit value are best estimated by the difference estimator (intercept only).
#' @field av_rat Audit value with many errors. Differences between book and
#' audit value are best estimated by the ratio estimator (slope only).
#'
#'
"inventoryData"

#' Salaries of 2,222 imaginary civil servants of Winesburg.
#'
#' A dataset containing grade, step, gross salary, employee ID, and gender of
#' 2,222 imaginary employees of the equally imaginary town of Winesburg.
#'
#' @format A data frame with 5 variables in the following order:
#' @field grade Grade of the individual. There are 20 grades. Civil servants
#'   may be promoted to a higher grade.
#' @field step Step within the grade. Civil servants earn an additional step
#'   within the grade until the maximum is reached.
#' @field gross Gross salary amount.
#' @field id Unique employee ID.
#' @field sex Gender. 0 indicates male, 1 indicates female.
#'
"salaries"
