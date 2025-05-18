#' Upper bound on the number of deviations.
#'
#' Calculates the upper bound on the number of deviations, using the
#' hypergeometric, binomial, or Poisson distribution.
#'
#' @param k Number of deviations found in the sample. Integer less than
#'   \code{n}.
#' @param popn Number of elements in the population. Positive integer.
#' @param n Sample size. Integer less than or equal to \code{popn}.
#' @param alpha Significance level.
#' @param dist Distribution used. Default distribution is "hyper". Other options
#'   available are "binom" and "pois".
#' @return The one-sided upper bound on the number of deviations in the
#'   population at a confidence level of \code{(1 - alpha)}.
#' @examples
#' upper(0, 1000000, 59, .05)
#' upper(0, 1000000, 59, .05, dist = "binom")
#' upper(0, 1000000, 59, .05, dist = "pois")
#' @export

upper <- function(k = 0, popn, n, alpha, dist = "hyper") {
  checkNumeric(k)
  checkPositive(k)
  checkInteger(k)
  checkLessEqthan(k, n)
  checkNumeric(n)
  checkPositive(n)
  checkInteger(n)
  checkBetween(alpha, 0, 1)
  checkOptions(dist, c("hyper", "binom", "pois"))

  if (dist == "pois") {
    if (missing(popn)) {
      x <- upperPois(n, k, alpha)
    } else {
      checkNumeric(popn)
      checkPositive(popn)
      checkInteger(popn)
      checkLessthan(n, popn)

      x <- ceiling(upperPois(n, k, alpha) * popn)
    }

  } else if (dist == "binom") {
    if (missing(popn)) {
      x <- upperBinom(n, k, alpha)
    } else {
      checkNumeric(popn)
      checkPositive(popn)
      checkInteger(popn)
      checkLessthan(n, popn)

      x <- ceiling(upperBinom(n, k, alpha) * popn)
    }

  } else if (dist == "hyper") {
    x <- ceiling(upperHyper(popn, n, k, alpha))
  }

  return(x)

}

#' Lower bound on the number of deviations.
#'
#' Calculates the lower bound on the number of deviations, using the
#' hypergeometric, binomial, or Poisson distribution.
#'
#' @param popn Number of elements in the population. Positive integer.
#' @param n Sample size. Integer less than or equal to \code{popn}.
#' @param k Number of deviations found in the sample. Integer less than
#'   \code{n}.
#' @param alpha Significance level.
#' @param dist Distribution used. Default distribution is "hyper". Other options
#'   available are "binom" and "pois".
#' @return The lower bound on the number of deviations in the population at a
#'   confidence level of \code{(1 - alpha)}.
#' @examples
#' lower(10, 1000000, 59, .05)
#' lower(10, 1000000, 59, .05, dist = "pois")
#' lower(10, 1000000, 59, .05, dist = "binom")
#' lower(k = 5, n = 59, alpha = .05, dist = "binom")
#' @export

lower <- function(k = 0, popn, n, alpha, dist = "hyper") {
  checkNumeric(k)
  checkPositive(k)
  checkInteger(k)
  checkLessEqthan(k, n)
  checkNumeric(n)
  checkPositive(n)
  checkInteger(n)
  checkBetween(alpha, 0, 1)
  checkOptions(dist, c("hyper", "binom", "pois"))

  if (dist == "pois") {
    if (missing(popn)) {
      x <- lowerPois(n, k, alpha)
    } else {
      checkNumeric(popn)
      checkPositive(popn)
      checkInteger(popn)
      checkLessthan(n, popn)

      x <- floor(lowerPois(n, k, alpha) * popn)
    }

  } else if (dist == "binom") {
    if (missing(popn)) {
      x <- lowerBinom(n, k, alpha)
    } else {
      checkNumeric(popn)
      checkPositive(popn)
      checkInteger(popn)
      checkLessthan(n, popn)

      x <- floor(lowerBinom(n, k, alpha) * popn)
    }

  } else if (dist == "hyper") {
    x <- floor(lowerHyper(popn, n, k, alpha))
  }

  return(x)

}
