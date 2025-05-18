#' Checks if variable input is positive
#'
#' Some functions throw errors when input variables have a negative value. This
#' then results in an R error message that is often incomprehensible to a user.
#'
#' This function traps negative input values, either from a single value or a
#' vector of numeric values, and presents an error message that better explains
#' the cause of the function to halt.
#'
#' @param var Variable. An input variable to a function.

checkPositive <- function(var) {
  if (any(var < 0))
    stop(paste(deparse(substitute(var)),
               "should be positive")
    )
}

#' Checks if variable input is not zero
#'
#' Some functions throw errors when input variables are equal to zero. This
#' then results in an R error message that is often incomprehensible to a user.
#'
#' This function traps zero input values, either from a single value or a
#' vector of numeric values, and presents an error message that better explains
#' the cause of the function to halt.
#'
#' @param var Variable. An input variable to a function.

checkNonzero <- function(var) {
  if (any(var == 0))
    stop(paste(deparse(substitute(var)),
               "includes values equal to zero")
    )
}

#' Checks if one variable is less than than another
#'
#' Some functions throw errors when one input variable is less than another.
#' This then results in an R error message that is often incomprehensible to a
#' user.
#'
#' This function traps when a variable is greater than or equal to another when
#' it should not, and presents an error message that better explains the cause
#' of the function to halt.
#'
#' @param smaller Variable. An input variable to a function.
#' @param greater Variable. An input variable to a function.

checkLessthan <- function(smaller, greater) {
  if (smaller >= greater)
    stop(paste(deparse(substitute(smaller)),
               "should be less than",
               deparse(substitute(greater)))
    )
}

#' Checks if one variable input is less than or equal to than another
#'
#' Some functions throw errors when one input variable is less than or equal to
#' another. This then results in an R error message that is often
#' incomprehensible to a user.
#'
#' This function traps when a variable is greater than another when it should
#' not, and presents an error message that better explains the cause of the
#' function to halt.
#'
#' @param smaller Variable. An input variable to a function.
#' @param greater Variable. An input variable to a function.

checkLessEqthan <- function(smaller, greater) {
  if (smaller > greater)
    stop(paste(deparse(substitute(smaller)),
               "should be less than or equal to",
               deparse(substitute(greater)))
    )
}

#' Checks if variable has value within interval
#'
#' Some functions throw errors when an input variable has a value outside a
#' certain interval. This then results in an R error message that is often
#' incomprehensible to a user.
#'
#' This function traps when a variable has a value outside the expected interval
#' and presents an error message that better explains the cause of the function
#' to halt.
#'
#' @param var Variable. An input variable to a function.
#' @param lower Variable. An input variable to a function, or a numeric value.
#' @param upper Variable. An input variable to a function, or a numeric value.

checkBetween <- function(var, lower, upper) {
  if (var <= lower | var >= upper)
    stop(paste(deparse(substitute(var)),
               "should be between",
               deparse(substitute(lower)),
               "and",
               deparse(substitute(upper)))
    )
}

#' Checks if variable is integer
#'
#' Some functions throw errors when an input variable doesn't have an integer
#' value. This then results in an R error message that is often incomprehensible
#' to a user.
#'
#' This function traps when a variable has a non-integer value, either from a
#' single value or a vector of numeric values, and presents an error message
#' that better explains the cause of the function to halt.
#'
#' @param var Variable. An input variable to a function.

checkInteger <- function(var) {
  if (!(class(var) %in% c("numeric", "integer")))
    stop(paste(deparse(substitute(var)),
               "should be numeric")
    )

  if (any(var %% 1 != 0))
    stop(paste(deparse(substitute(var)),
               "should be integer")
    )
}

#' Checks if variable has value from list of options
#'
#' Some functions throw errors when an input variable has a value assigned to it
#' that isn't part of a list of valid options. This then results in an R error
#' message that is often incomprehensible to a user.
#'
#' This function traps when a variable has an invalid option and presents an
#' error message that better explains the cause of the function to halt.
#'
#' @param var Variable. An input variable to a function.
#' @param options Options. A vector with character string options.

checkOptions <- function(var, options) {
  if (!(var %in% options))
    stop(paste(deparse(substitute(var)),
               "has invalid option")
    )
}

#' Check if variables are either positive or negative
#'
#' Some functions throw errors when an input vector has a mixture of positive
#' and negative values assigned to it. This then results in an R error message
#' that is often incomprehensible to a user.
#'
#' This function traps when a vector contains both negative and positive values
#' and presents an error message that better explains the cause of the function
#' to halt.
#'
#' @param var Variable. An input variable to a function.

checkStrictsign <- function(var) {
  if (any(var > 0) & any(var < 0))
    stop(paste(deparse(substitute(var)),
               "contains positive and negative values. Run split_population
               first and then select from either positive or negative values")
    )
}

#' Check input vectors for equal length
#'
#' Some functions throw errors when two input vectors have different lengths.
#' This then results in an R error message that is often incomprehensible to a
#' user.
#'
#' This function traps when input vectors have different lengths and presents an
#' error message that better explains the cause of the function to halt.
#'
#' @param vec1 Vector. An input variable to a function.
#' @param vec2 Vector. An input variable to a function.

checkLengths <- function(vec1, vec2) {
  if (length(vec1) != length(vec2))
    stop(paste(deparse(substitute(vec1)),
               "and",
               deparse(substitute(vec2)),
               "should have the same length")
    )
}

#' Check if inputs are numeric
#'
#' Some functions throw errors when input vectors are not numeric.
#' This then results in an R error message that is often incomprehensible to a
#' user.
#'
#' This function traps when input vectors are not numeric and presents an
#' error message that better explains the cause of the function to halt.
#'
#' @param var Variable An input variable or vector to a function.

checkNumeric <- function(var) {
  if (!is.numeric(var))
    stop(paste(deparse(substitute(var)),
               "should be numeric")
    )
}

#' Check if elements in a vector are unique
#'
#' Some functions require that a vector contains unique values. If this is not
#' the case, the code may not function correctly, the functionality may be
#' jeopardized, and/or an R error message is produced that is often
#' incomprehensible to a user.
#'
#' This function traps when input vectors are not unique and presents an error
#' message that better explains the cause of the function to halt.
#'
#' @param var Variable An input variable or vector to a function.

checkUnique <- function(var) {
  if (any(duplicated(var)) == TRUE)
    stop(paste(deparse(substitute(var)),
               "contains duplicated values")
    )
}
