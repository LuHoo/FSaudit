#' MUS size calculation.
#'
#' @description  Calculates the sample size for a monetary unit sample, using
#'   the hypergeometric distribution with the expected error as an argument.
#'
#' @param cl Confidence level. A number greater than 0 and less than 1.
#' @param bv Population book value. It could be either a vector of book values,
#'   from which the total book value is computed as the rounded sum of book
#'   values for subsequent computations, or the total population book value.
#' @param pm Performance materiality.
#' @param ee Expected error.
#' @param evalMeth Evaluation method.
#' @return The minimum sample size required to conclude with \code{cl}
#'   confidence that the true monetary error does not exceed \code{pm} if the
#'   projected error doesn't exceed \code{ee}. Calculations use the
#'   hypergeometric distribution , consistent with sampling from a finite
#'   population without replacement.
#' @export
musSizeHyper <- function(cl, bv, pm, ee, evalMeth) {

  checkBetween(cl, 0, 1)
  checkNumeric(cl)
  checkPositive(bv)
  checkNumeric(pm)
  checkPositive(pm)
  checkInteger(pm)
  checkNumeric(ee)
  checkPositive(ee)
  checkInteger(ee)
  checkLessthan(ee, pm)

  popBv <- round(as.numeric(sum(bv)), 0)

  if (evalMeth == "cell") {

    n <- findSizeCell(cl, popBv, pm, ee, dist = "hyper")

  } else {

    res <- sizeHyperEe(alpha = 1 - cl,
                       popdev = pm,
                       popn = popBv,
                       ee = ee)

    k <- max(res$c, 0)
    nk <- res$nprev
    nk1 <- res$n

    if (k > 0) {
      n <- ceiling((k - 1 - (nk) / (nk1 - nk)) / (ee / popBv - 1 / (nk1 - nk)))
    } else {
      n <- res$n
    }
  }

  return(n)
}
