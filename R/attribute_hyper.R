#' Sample size hypergeometric.
#'
#' Calculates the sample size for a fixed attribute sample, using the
#' hypergeometric distribution.
#'
#' @param alpha Significance level. A number greater than 0 and less than 1.
#' @param popdev Number of deviations in the population. A positive integer less
#'   than \code{popn}.
#' @param popn Number of items in population. A positive integer.
#' @param c Critical number of deviations. A positive integer less than
#'   \code{popdev}.
#' @return The minimum sample size required to conclude with \code{1 - alpha}
#'   confidence that the true number of deviations does not exceed \code{tdr} if
#'   no more than \code{c} deviations are found in the sample. Calculations use
#'   the hypergeometric distribution.

sizeHyper <- function(alpha, popdev, popn, c) {
  checkNumeric(alpha)
  checkBetween(alpha, 0, 1)
  checkNumeric(popdev)
  checkPositive(popdev)
  checkInteger(popdev)
  checkNumeric(popn)
  checkPositive(popn)
  checkInteger(popn)
  checkNumeric(c)
  checkPositive(c)
  checkInteger(c)

  if (popdev >= popn) {
    n <- 0
    n
  }

  n <- sizePois(alpha, popdev / popn, c);
  n <- min(n, popn - popdev);
  sizeHyperHelper(alpha, popdev, popn, c, n);
}


sizeHyperHelper <- function(alpha, popdev, popn, c, n) {

  prob <- phyper(c, popdev, popn - popdev, n)
  if (prob >= alpha) {
    c1 <- 1
    c2 <- 1
    p0 <- phyper(c, popdev, popn - popdev, n + 1)
  } else {
    c1 <- -1
    c2 <- 0
    p0 <- phyper(c, popdev, popn - popdev, n - 1)
  }
  if (prob >= alpha && p0 < alpha | prob < alpha && p0 > alpha) {
    return(n + c2)
  } else {
    sizeHyperHelper(alpha, popdev, popn, c, n + c1)
  }
}


#' Calculate the sample size for a fixed attribute sample, using the
#' hypergeometric distribution when expected error is used as an argument.
#'
#' @param alpha Significance level. A number greater than 0 and less than 1.
#' @param popdev Number of deviations in the population.
#' @param popn Number of items in population.
#' @param ee Expected error.
#' @return A list containing the sample size \code{n}, the critical number
#'   \code{c} associated with the sample size, and the sample size \code{nprev}
#'   associated with a critical number \code{c - 1}. The minimum sample size
#'   required to conclude with \code{1 - alpha} confidence that the true
#'   deviation rate does not exceed \code{tdr} if no more than \code{ceiling(n *
#'   eer)} deviations are found in the sample. Calculations use the
#'   hypergeometric distribution.

sizeHyperEe <- function(alpha, popdev, popn, ee) {

  checkBetween(alpha, 0, 1)
  checkPositive(popdev)
  checkInteger(popdev)
  checkPositive(popn)
  checkInteger(popn)
  checkLessthan(ee, popdev)
  checkPositive(ee)
  checkInteger(ee)

  c <- 0
  n <- sizeHyper(alpha = alpha, popdev = popdev, popn = popn, c = c)
  mle <- n * ee / popn
  nprev <- 0

  while (mle > c) {
    c <- c + 1
    nprev <- n
    n <- sizeHyper(alpha = alpha, popdev = popdev, popn = popn, c = c)
    mle <- n * ee / popn

  }
  result <- list(n = n, c = c, nprev = nprev)
  return(result)

}

#' Hypergeometric upper bound.
#'
#' Calculates the upper bound on the number of deviations using the
#' hypergeometric distribution, for sampling without replacement.
#'
#' @param popn Number of elements in the population. Positive integer.
#' @param n Sample size. Positive integer less than or equal to \code{popn}.
#' @param k Number of deviations found in the sample. Positive nteger less than
#'   or equal to \code{n}.
#' @param alpha Significance level. A number greater than 0 and less than 1.
#' @return The upper bound on the number of deviations in the population at a
#'   confidence level of \code{1 - alpha}.

upperHyper <- function(popn, n, k = 0, alpha) {
  checkNumeric(popn)
  checkPositive(popn)
  checkInteger(popn)
  checkNumeric(n)
  checkLessEqthan(n, popn)
  checkPositive(n)
  checkInteger(n)
  checkNumeric(k)
  checkLessEqthan(k, n)
  checkPositive(k)
  checkInteger(k)
  checkNumeric(alpha)
  checkBetween(alpha, 0, 1)

  m <- ceiling(upperBinom(n, k, alpha) * popn)

  if (k == n) {
    return(popn)
  } else {
    prob <- phyper(k, m, popn - m, n)
    while (prob <= alpha) {
      m <- m - 1
      prob <- phyper(k, m, popn - m, n)
    }
    return(m)
  }
}


#' Hypergeometric lower bound.
#'
#' Calculates the lower bound on the number of deviations using the
#' hypergeometric distribution, for sampling without replacement.
#'
#' @param popn Number of elements in the population. Positive integer.
#' @param n Sample size. Positive integer less than or equal to \code{popn}.
#' @param k Number of deviations found in the sample. Positive integer less than
#'   or equal to \code{n}.
#' @param alpha Significance level. A number greater than 0 and less than 1.
#' @return The lower bound on the number of deviations in the population at a
#'   confidence level of \code{1 - alpha}.

lowerHyper <- function(popn, n, k = 0, alpha) {
  checkNumeric(popn)
  checkPositive(popn)
  checkInteger(popn)
  checkNumeric(n)
  checkLessEqthan(n, popn)
  checkPositive(n)
  checkInteger(n)
  checkNumeric(k)
  checkLessEqthan(k, n)
  checkPositive(k)
  checkInteger(k)
  checkNumeric(alpha)
  checkBetween(alpha, 0, 1)

  m <- floor(lowerBinom(n, k, alpha) * popn)
  if (k == 0) {
    return(0)
  } else {
    prob <- 1 - phyper(k - 1, m, popn - m, n)
    if (prob <= alpha) {
      while (prob <= alpha) {
        m <- m + 1
        prob <- 1 - phyper(k - 1, m, popn - m, n)
      }
      return(m)
    } else {
      while (prob > alpha) {
        m <- m - 1
        prob <- 1 - phyper(k - 1, m, popn - m, n)
      }
      return(m + 1)
    }
  }
}
