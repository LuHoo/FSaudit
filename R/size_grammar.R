#' Sample size calculation.
#'
#' @description The \code{size} function is a generic function for sample size
#'   calculation. The function invokes particular \emph{methods} which depend on
#'   the class of the first argument.
#'
#' @details The \code{size} function calculates the sample size for a MUS, CVS
#'   or attribute sample.
#'
#'   MUS sample sizes are calculated using the confidence level, total
#'   population book value, performance materiality, and the expected
#'   misstatement. The default distribution is the hypergeometric,
#'   approximations using the binomial or Poisson distribution are available as
#'   well.
#'
#'   Attribute sample sizes are calculated using the significance level, the
#'   number of tolerable deviations in the population, the number of items in
#'   the population, and either the expected deviation rate or the critical
#'   number.
#'
#'   Classical variables sample sizes are calculated using the confidence level,
#'   the required precision, and the estmated standard deviations of the audit
#'   values. If these are omitted, the function uses the standard deviations of
#'   the book values instead.
#'
#' @param state State object. The \code{size} method is applicable to
#'   \code{att_obj}, \code{mus_obj} and \code{cvs_obj} objects.
#' @param ... Further arguments passed to or from other methods.
#'
#' @rdname size
#' @export
size <- function(state, ...) UseMethod("size")


#' @rdname size
#' @export
size.musStateObject <- function(state, ...) {

  state    <- modifyState(state, lazyeval::lazy_dots(...))
  state$n  <- mus_size(state$cl,
                       state$popBv,
                       state$pm,
                       state$ee,
                       state$dist,
                       state$evalMeth)
  state
}


#' @rdname size
#' @export
size.cvsStateObject <- function(state, ...) {

  state       <- modifyState(state, lazyeval::lazy_dots(...))

  sizeResults <- cvs_size(state$bv,
                          state$strat,
                          state$strata,
                          state$stratMeth,
                          state$cl,
                          state$desPrec,
                          state$sdAv,
                          state$alloc)
  state$alloc <- sizeResults$alloc
  state$n     <- sizeResults$n

  state

}

#' @rdname size
#' @export
size.attStateObject <- function(state, ...) {

  state <- modifyState(state, lazyeval::lazy_dots(...))
  if (state$dist == "hyper") {
    if (is.null(state$ee) & !is.null(state$c)) {
      state$n <- sizeHyper(alpha = state$alpha,
                           popdev = state$popdev,
                           popn = state$popn,
                           c = state$c)
    } else {
      res     <- sizeHyperEe(alpha = state$alpha,
                         popdev = state$popdev,
                         popn = state$popn,
                         ee = state$ee)

      state$n <- res$n
      state$c <- res$c
    }
  }
  if (state$dist == "binom") {
    if (is.null(state$eer) & !is.null(state$c)) {
      n <- sizeBinom(alpha = state$alpha,
                     tdr   = state$tdr,
                     c     = state$c)
      state$n <- min(n, state$popn)
    } else {
      res     <- sizeBinomEer(alpha = state$alpha,
                              tdr = state$tdr,
                              eer = state$eer)
      state$n <- min(res$n, state$popn)
      state$c <- res$c
    }
  }

  if (state$dist == "pois") {
    if (is.null(state$eer) & !is.null(state$c)) {
      n       <- sizePois(alpha = state$alpha,
                          tdr = state$tdr,
                          c = state$c)
      state$n <- min(n, state$popn)
    } else {
      res <- sizePoisEer(alpha = state$alpha,
                         tdr = state$tdr,
                         eer = state$eer)
      state$n <- min(res$n, state$popn)
      state$c <- res$c
    }
  }
  state
}
