#' Sample size Poisson.
#'
#' Calculates the sample size for a fixed attribute sample, using the Poisson
#' distribution, with the number of critical deviations as an argument.
#'
#' @param alpha Significance level. A number greater than 0 and less than 1.
#' @param tdr Tolerable deviation rate. A number greater than 0 and less than 1.
#' @param c Critical number of deviations. A positive integer.
#' @return The minimum sample size required to conclude with \code{1 - alpha}
#'   confidence that the true deviation rate does not exceed \code{tdr} if no
#'   more than \code{c} deviations are found in the sample. Calculations use the
#'   Poisson approximation, assuming \code{tdr} approaching 0 and population
#'   size approaching infinity.

sizePois <- function(alpha, tdr, c) {
  checkNumeric(alpha)
  checkBetween(alpha, 0, 1)
  checkNumeric(tdr)
  checkBetween(tdr, 0, 1)
  checkNumeric(c)
  checkPositive(c)
  checkInteger(c)

  ceiling(qgamma(1 - alpha, c + 1) / tdr)
}

#' Sample size Poisson.
#'
#' Calculates the sample size for a fixed attribute sample, using the Poisson
#' approximation, with the expected error rate as an argument.
#'
#' @param alpha Significance level. A number greater than 0 and less than 1.
#' @param tdr Tolerable deviation rate.  A number greater than 0 and less than
#'   1.
#' @param eer Expected error rate.  A number greater than 0 and less than 1. The
#'   expected error rate should be less than the tolerable deviation rate.
#' @return A list containing the sample size \code{n}, the critical number
#'   \code{c} associated with the sample size, and the sample size \code{nprev}
#'   associated with a critical number \code{c - 1}. The minimum sample size
#'   required to conclude with \code{1 - alpha} confidence that the true
#'   deviation rate does not exceed \code{tdr} if no more than \code{ceiling(n *
#'   eer)} deviations are found in the sample. Calculations use the Poisson
#'   approximation, assuming \code{tdr} approaching 0 and population size
#'   approaching infinity.

sizePoisEer <- function(alpha, tdr, eer) {
  checkNumeric(alpha)
  checkBetween(alpha, 0, 1)
  checkNumeric(eer)
  checkPositive(eer)
  checkNumeric(tdr)
  checkPositive(tdr)
  checkLessthan(eer, tdr)
  checkLessthan(tdr, 1)

  c <- 0
  n <- sizePois(alpha = alpha, tdr = tdr, c = c)
  mle <- n * eer
  nprev <- 0

  while (mle > c) {
    c <- c + 1
    nprev <- n
    n <- sizePois(alpha = alpha, tdr = tdr, c = c)
    mle <- n * eer

  }
  res <- list(n = n, c = c, nprev = nprev)
  return(res)
}

#' Poisson upper bound.
#'
#' Calculates the upper bound on the fraction of deviations using the Poisson
#' distribution, for sampling from infinite populations or as an approximation
#' for sampling without replacement.
#'
#' @param n Sample size. Positive integer.
#' @param k Number of deviations found in the sample. Integer less than or equal
#'   to \code{n}.
#' @param alpha Significance level. A number greater than 0 and less than 1.
#' @return The one-sided upper bound on the fraction of deviations in the
#'   population at a confidence level of \code{1 - alpha}.

upperPois <- function(n, k = 0, alpha) {
  checkNumeric(n)
  checkInteger(n)
  checkPositive(n)
  checkNumeric(k)
  checkLessEqthan(k, n)
  checkInteger(k)
  checkPositive(k)
  checkNumeric(alpha)
  checkBetween(alpha, 0, 1)

  stats::qgamma(1 - alpha, k + 1) / n
}

#' Poisson lower bound.
#'
#' Calculates the lower bound on the fraction of deviations using the Poisson
#' distribution, for sampling from infinite populations or as an approximation
#' for sampling without replacement.
#'
#' @param n Sample size. Positive integer.
#' @param k Number of deviations found in the sample. Integer less than or equal
#'   to \code{n}.
#' @param alpha Significance level. A number greater than 0 and less than 1.
#' @return The one-sided lower bound on the fraction of deviations in the
#'   population at a confidence level of \code{1 - alpha}.

lowerPois <- function(n, k = 0, alpha) {
  checkNumeric(n)
  checkInteger(n)
  checkPositive(n)
  checkNumeric(k)
  checkLessEqthan(k, n)
  checkInteger(k)
  checkPositive(k)
  checkNumeric(alpha)
  checkBetween(alpha, 0, 1)

  stats::qgamma(alpha, k) / n
}
