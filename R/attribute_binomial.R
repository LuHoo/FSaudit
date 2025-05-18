#' Sample size binomial.
#'
#' Calculate the sample size for a fixed attribute sample, using the binomial
#' approximation, with the number of critical deviations as an argument.
#'
#' @param alpha Significance level. A number greater than 0 and less than 1.
#' @param tdr Tolerable deviation rate. A number between 0 and 1.
#' @param c Critical number of deviations. A positive integer.
#' @return The minimum sample size required to conclude with \code{cl}
#'   confidence that the true deviation rate does not exceed \code{tdr} if no
#'   more than \code{c} deviations are found in the sample. Calculations use the
#'   binomial approximation.

sizeBinom <- function(alpha, tdr, c) {
  checkNumeric(alpha)
  checkBetween(alpha, 0, 1)
  checkNumeric(tdr)
  checkBetween(tdr, 0, 1)
  checkNumeric(c)
  checkPositive(c)
  checkInteger(c)

  # equivalent to qgamma in the poisson scenario
  n <- sizePois(alpha, tdr, c);
  sizeBinomHelper(alpha, tdr, c, n)
}

sizeBinomHelper <- function(alpha, tdr, c, n) {
  if (pbinom(c, n, tdr) > alpha) {
    n + 1
  } else {
    sizeBinomHelper(alpha, tdr, c, n - 1)
  }
}

#' Sample size binomial.
#'
#' Calculates the sample size for a fixed attribute sample, using the binomial
#' approximation, with expected error rate as an argument.
#'
#' @param alpha Significance level. A number greater than 0 and less than 1.
#' @param tdr Tolerable deviation rate. A number greater than 0 and less than 1.
#' @param eer Expected error rate. A number greater than 0 and less than 1, but
#'   less than \code{tdr}.
#' @return A list containing the sample size \code{n}, the critical number
#'   \code{c} associated with the sample size, and the sample size \code{nprev}
#'   associated with a critical number \code{c - 1}. The minimum sample size
#'   required to conclude with \code{1 - alpha} confidence that the true
#'   deviation rate does not exceed \code{tdr} if no more than \code{ceiling(n *
#'   eer)} deviations are found in the sample. Calculations use the binomial
#'   approximation.

sizeBinomEer <- function(alpha, tdr, eer) {
  checkNumeric(alpha)
  checkBetween(alpha, 0, 1)
  checkNumeric(eer)
  checkPositive(eer)
  checkNumeric(tdr)
  checkPositive(tdr)
  checkLessthan(eer, tdr)
  checkLessthan(tdr, 1)

  c <- 0
  n <- sizeBinom(alpha = alpha, tdr = tdr, c = c)
  mle <- n * eer
  nprev <- 0

  while (mle > c) {
    c <- c + 1
    nprev <- n
    n <- sizeBinom(alpha = alpha, tdr = tdr, c = c)
    mle <- n * eer

  }
  res <- list(n = n, c = c, nprev = nprev)
  return(res)
}

#' Binomial upper bound.
#'
#' Calculates the upper bound on the fraction of deviations using the binomial
#' distribution, for sampling with replacement or as an approximation for
#' sampling without replacement.
#'
#' @param n Sample size. Positive integer.
#' @param k Number of deviations found in the sample. Integer less than or equal
#'   to \code{n}.
#' @param alpha Significance level. A number greater than 0 and less than 1.
#' @return The one-sided upper bound on the fraction of deviations in the
#'   population at a confidence level of \code{(1 - alpha)}.

upperBinom <- function(n, k = 0, alpha) {
  checkNumeric(n)
  checkInteger(n)
  checkPositive(n)
  checkNumeric(k)
  checkLessEqthan(k, n)
  checkInteger(k)
  checkPositive(k)
  checkNumeric(alpha)
  checkBetween(alpha, 0, 1)

  cl <- 1 - alpha
  upper <- stats::qbeta(cl, k + 1, n - k)
  upper
}

#' Binomial lower bound.
#'
#' Calculates the lower bound on the fraction of deviations using the binomial
#' distribution, for sampling with replacement or as an approximation for
#' sampling without replacement.
#'
#' @param n Sample size. Positive integer.
#' @param k Number of deviations found in the sample. Integer less than
#'   \code{n}.
#' @param alpha Significance level. A number greater than 0 and less than 1.
#' @return The one-sided lower bound on the fraction of deviations in the
#'   population at a confidence level of \code{(1 - alpha)}.

lowerBinom <- function(n, k = 0, alpha) {
  checkNumeric(n)
  checkInteger(n)
  checkPositive(n)
  checkNumeric(k)
  checkLessEqthan(k, n)
  checkInteger(k)
  checkPositive(k)
  checkNumeric(alpha)
  checkBetween(alpha, 0, 1)

  lower <- stats::qbeta(alpha, k, n - k + 1)
  lower
}
