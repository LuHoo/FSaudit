#' MUS size calculation.
#'
#' @description  Calculates the sample size for a monetary unit sample, using
#'   the binomial approximation with the expected error as an argument.
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
#'   projected error doesn't exceed \code{ee}. Calculations use the binomial
#'   approximation, assuming \code{pm/popBv} to be less than .05 and the sample
#'   size \code{n} to be greater than 20.
#' @examples
#' musSizeBinom(.90, inventoryData$bv, 100000, 20000)
#' musSizeBinom(.90, inventoryData$bv, 100000, 0)
#' musSizeBinom(.90, 5000000, 100000, 0)
#' musSizeBinom(.90, 5e06, 1e05, 0)
#' @export

musSizeBinom <- function(cl, bv, pm, ee, evalMeth = "cell") {
  checkNumeric(cl)
  checkBetween(cl, 0, 1)
  checkPositive(bv)
  checkStrictsign(bv)
  checkNumeric(pm)
  checkPositive(pm)
  checkInteger(pm)
  checkNumeric(ee)
  checkPositive(ee)
  checkInteger(ee)
  checkLessthan(pm, sum(bv))
  checkLessthan(ee, pm)

  popBv <- abs(round(as.numeric(sum(bv)), 0))

  if (evalMeth == "cell") {

    n <- findSizeCell(cl, popBv, pm, ee, dist = "binom")

  } else {

    res <- sizeBinomEer(alpha = 1 - cl,
                        tdr = pm / sum(bv),
                        eer = ee / sum(bv))

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
