#' Sample evaluation.
#'
#' @description The \code{evaluate} function is a generic function for sample
#'   evaluation. The function invokes particular \emph{methods} which depend on
#'   the class of the first argument.
#' @details Most sampling methods store information of the selected sampling
#'   units, like a unique identifier and the book value. The auditor then
#'   obtains the true (audited) values of the sampling units, these
#'   \code{av} are submitted through the \code{evaluate} function.
#'   Audit values should be in the same order as the book values.
#'
#'   \code{evaluate.musStateObject} produces a projected misstatement and
#'   one-sided upper bound on the misstatement amount, separately for
#'   overstatements and understatements.
#'
#'   The method uses either the \strong{cell evaluation} method (the default),
#'   or the \strong{Stringer bound} method.
#'
#'   If significant items were removed prior to selecting the sampling units,
#'   the evaluation pertains only to the total value of the population exclusive
#'   of the significant items.
#'
#'   The evaluation results are calculated with the default
#'   \code{hypergeometric} distribution. Evaluation with the approximating
#'   binomial or Poisson distribution are optional.
#'
#'   \code{evaluate.cvsStateObject} produces a projected misstatement and
#'   achieved precision for four different evaluation methods.
#'
#' @param state State object. This typically is an \code{mus_obj} object. The
#'   \code{significant} method is not defined for other state objects.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return The form of the value returned by \code{evaluate} depends on the
#'   class of its argument. If applied on a \code{cvs_obj} the function returns
#'   a list with results from the evaluation, including the effective number of
#'   degrees of freedom, and for each of the estimation methods MPU, difference,
#'   ratio or regression estimation the point estimate of the true population
#'   value, the point estimate of the error value, and the achieved precision.
#'   In addition, for each of the estimation methods a summary dataframe is
#'   provided with the sample size, number of nonzero differences, mean error
#'   value and estimated regressor standard deviation per stratum.
#'
#'   If applied on a \code{mus_obj} the function returns a list with results
#'   from the evaluation. For \code{cell} and \code{stringer}, the list contains
#'   a general section with the confidence level, sampling interval, and basic
#'   precision, as well as separate sections for overstatement and
#'   understatement evaluation, that contain the sample size, number of nonzero
#'   misstatements, projected misstatement, precision gap widening, allowance
#'   for sampling risk, and the upper precision limit. For PPS estimation, the
#'   list contains the projected misstatement, the upper bound on misstatements,
#'   lower bound on misstatements, and achieved precision.
#' @rdname evaluate
#' @export
evaluate <- function(state, ...) UseMethod("evaluate")


#' @rdname evaluate
#' @export
evaluate.musStateObject <- function(state, ...) {
  state <- modifyState(state, lazyeval::lazy_dots(...))

  state$evalResults <-  mus_eval(state$cl,
                                 state$sample$bv,
                                 state$av,
                                 state$popBv,
                                 state$dist,
                                 state$evalMeth,
                                 state$aggregates,
                                 state$bvTested)


  state
}

#' @rdname evaluate
#' @export
evaluate.cvsStateObject <- function(state, ...) {

  state <- modifyState(state, lazyeval::lazy_dots(...))

  state$evalResults <- cvs_eval(state$av,
                                state$sample,
                                state$alloc,
                                state$popBv,
                                state$cl)

  state

}
